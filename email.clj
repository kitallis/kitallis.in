#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.data.xml :as xml]
         '[markdown.core :as md]
         '[org.httpkit.client :as http]
         '[cheshire.core :as json])

;; -- Config --

(def posts-dir "blog/posts")
(def sent-file "blog/.sent")

(defn env! [k]
  (or (System/getenv k)
      (do (println (str "Set " k " env var"))
          (System/exit 1))))

;; -- Parsing --

(def ^:private iso-fmt (java.text.SimpleDateFormat. "yyyy-MM-dd"))

(defn parse-post [file]
  (let [content (slurp (str file))
        filename (str (fs/file-name file))
        {:keys [metadata html]} (md/md-to-html-string-with-meta content)
        has-date? (re-find #"^\d{4}-\d{2}-\d{2}-" filename)
        date (if-let [d (:date metadata)]
               (.format iso-fmt d)
               (when has-date? (subs filename 0 10)))
        name-part (if has-date?
                    (subs filename 11 (- (count filename) 3))
                    (subs filename 0 (- (count filename) 3)))]
    {:title (or (:title metadata) name-part)
     :slug name-part
     :date date
     :html html}))

;; -- Sent tracking --

(defn sent-slugs []
  (if (fs/exists? sent-file)
    (set (remove str/blank? (str/split-lines (slurp sent-file))))
    #{}))

(defn mark-sent! [slug]
  (spit sent-file (str slug "\n") :append true))

;; -- Listmonk API --

(defn api [method path body]
  (let [api-key (env! "LISTMONK_API_KEY")
        api-user (env! "LISTMONK_API_USER")
        listmonk-url (env! "LISTMONK_URL")
        auth (str "Basic " (.encodeToString (java.util.Base64/getEncoder) (.getBytes (str api-user ":" api-key))))
        resp (http/request
              {:method method
               :url (str listmonk-url "/api" path)
               :headers {"Content-Type" "application/json"
                         "Authorization" auth}
               :body (when body (json/generate-string body))})]
    (let [{:keys [status body]} @resp]
      (when (>= status 400)
        (println (str "API error " status ": " body))
        (System/exit 1))
      (json/parse-string body true))))

;; -- HTML prep --

(defn escape-html [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn math-node->html [node]
  (if (string? node)
    (escape-html node)
    (let [{:keys [tag content]} node
          children (->> content
                        (mapv math-node->html)
                        (remove #(and (string? %) (str/blank? %)))
                        vec)]
      (case tag
        :msup       (str (first children) "<sup>" (second children) "</sup>")
        :msub       (str (first children) "<sub>" (second children) "</sub>")
        :mfrac      (str "(" (first children) "/" (second children) ")")
        :mrow       (apply str children)
        :munderover (str (first children) "<sub>" (second children) ".." (nth children 2) "</sub>")
        :munder     (str (first children) "<sub>" (second children) "</sub>")
        :mover      (str (first children) "<sup>" (second children) "</sup>")
        :math       (apply str children)
        (apply str children)))))

(defn prepare-html [html]
  (-> html
      (str/replace #"src=\"(/[^\"]+)\"" "src=\"https://kitallis.in$1\"")
      (str/replace #"<div class=\"section-break\">[^<]*</div>" "<hr>")
      (str/replace #"(?s)<math[^>]*>.*?</math>"
                   (fn [match]
                     (try (math-node->html (xml/parse-str match))
                          (catch Exception _ match))))))

;; -- Commands --

(defn send! [slug]
  (when (str/blank? slug)
    (println "Usage: bb email.clj send <slug>")
    (System/exit 1))
  (let [list-id (parse-long (env! "LISTMONK_LIST_ID"))
        files (fs/glob posts-dir "*.md")
        post (->> files (map parse-post) (filter #(= slug (:slug %))) first)]
    (when-not post
      (println (str "Post not found: " slug))
      (System/exit 1))
    (when (contains? (sent-slugs) slug)
      (println (str "Already sent: " slug))
      (System/exit 0))
    (let [campaign (api :post "/campaigns"
                        {:name (:title post)
                         :subject (:title post)
                         :body (prepare-html (:html post))
                         :content_type "html"
                         :type "regular"
                         :lists [list-id]})
          id (get-in campaign [:data :id])]
      (println (str "Created campaign #" id ": " (:title post)))
      (println "Start campaign? [y/n]")
      (when (= "y" (str/trim (or (read-line) "")))
        (api :put (str "/campaigns/" id "/status") {:status "running"})
        (mark-sent! slug)
        (println "Campaign sent.")))))

(defn send-unsent! []
  (let [list-id (parse-long (env! "LISTMONK_LIST_ID"))
        sent (sent-slugs)
        posts (->> (fs/glob posts-dir "*.md")
                   (map parse-post)
                   (remove #(contains? sent (:slug %))))]
    (if (empty? posts)
      (println "No unsent posts.")
      (doseq [{:keys [slug title html]} posts]
        (println (str "Sending: " title " (" slug ")"))
        (let [campaign (api :post "/campaigns"
                            {:name title
                             :subject title
                             :body html
                             :content_type "html"
                             :type "regular"
                             :lists [list-id]})
              id (get-in campaign [:data :id])]
          (api :put (str "/campaigns/" id "/status") {:status "running"})
          (mark-sent! slug)
          (println (str "  Campaign #" id " sent.")))))))

;; -- CLI --

(let [[cmd & args] *command-line-args*]
  (case cmd
    "send" (send! (first args))
    "send-unsent" (send-unsent!)
    (println "Usage: bb email.clj [send <slug>|send-unsent]")))

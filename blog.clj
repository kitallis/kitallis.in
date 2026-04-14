#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[markdown.core :as md]
         '[babashka.http-server :as server])

;; -- Config --

(def blog-dir "blog")
(def posts-dir (str blog-dir "/posts"))
(def drafts-dir (str blog-dir "/drafts"))
(def images-dir (str blog-dir "/images"))
(def templates-dir (str blog-dir "/templates"))
(def port 1313)

;; -- Templates --

(def index-template (slurp (str templates-dir "/index.html")))
(def post-template (slurp (str templates-dir "/post.html")))
(def post-link-template (slurp (str templates-dir "/post-link.html")))
(def rss-template (slurp (str templates-dir "/rss.xml")))
(def rss-item-template (slurp (str templates-dir "/rss-item.xml")))
(defn render [template vars]
  (reduce-kv (fn [html k v]
               (str/replace html (str "{{" (name k) "}}") (or v "")))
             template vars))

;; -- Parsing --

(defn parse-frontmatter [content]
  (when (str/starts-with? content "---\n")
    (let [end (str/index-of content "\n---\n" 4)]
      (when end
        {:meta (->> (subs content 4 end)
                    str/split-lines
                    (keep (fn [line]
                            (when-let [[_ k v] (re-matches #"(\w[\w-]*):\s+(.*)" line)]
                              [(keyword k) (str/trim v)])))
                    (into {}))
         :body (subs content (+ end 5))}))))

(defn ->rfc822 [date-str]
  (let [ld (java.time.LocalDate/parse date-str)
        zdt (.atStartOfDay ld (java.time.ZoneOffset/UTC))]
    (.format zdt java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(def ^:private display-date-fmt
  (java.time.format.DateTimeFormatter/ofPattern "MMMM d, yyyy"))

(defn ->display-date [date-str]
  (.format (java.time.LocalDate/parse date-str) display-date-fmt))

(defn parse-post [file draft?]
  (let [content (slurp (str file))
        filename (str (fs/file-name file))
        {:keys [meta body]} (or (parse-frontmatter content)
                                {:meta {} :body content})
        has-date? (re-find #"^\d{4}-\d{2}-\d{2}-" filename)
        filename-date (when has-date? (subs filename 0 10))
        date (or (:date meta) filename-date (str (java.time.LocalDate/now)))
        name-part (if has-date?
                    (subs filename 11 (- (count filename) 3))
                    (subs filename 0 (- (count filename) 3)))]
    {:title (or (:title meta) name-part)
     :date date
     :display-date (->display-date date)
     :slug name-part
     :name-part name-part
     :draft? draft?
     :html (md/md-to-html-string body)}))

;; -- Build --

(defn load-posts [include-drafts?]
  (let [posts (when (fs/exists? posts-dir)
                (->> (fs/glob posts-dir "*.md")
                     (mapv #(parse-post % false))))
        drafts (when (and include-drafts? (fs/exists? drafts-dir))
                 (->> (fs/glob drafts-dir "*.md")
                      (mapv #(parse-post % true))))]
    (into (vec posts) drafts)))

(def draft-tag-template (slurp (str templates-dir "/draft-tag.html")))

(defn draft-tag [draft?]
  (if draft? draft-tag-template ""))

(defn post-link [{:keys [title display-date slug draft?]}]
  (render post-link-template
          {:slug slug :date display-date :title title :draft-tag (draft-tag draft?)}))

(defn clean! []
  (when (fs/exists? "index.html")
    (fs/delete "index.html"))
  (when (fs/exists? (str blog-dir "/rss.xml"))
    (fs/delete (str blog-dir "/rss.xml")))
  (when (fs/exists? (str blog-dir "/index.html"))
    (fs/delete (str blog-dir "/index.html")))
  (when (fs/exists? "p")
    (fs/delete-tree "p")))

(defn build! [include-drafts?]
  (clean!)
  (let [posts (load-posts include-drafts?)
        sorted (sort-by :date #(compare %2 %1) posts)
        n-drafts (count (filter :draft? posts))]
    ;; index (root page)
    (spit "index.html"
          (render index-template
                  {:posts (str/join "\n      " (map post-link sorted))}))
    ;; posts
    (doseq [{:keys [slug title display-date html draft?] :as post} posts]
      (let [dir (str "p/" slug)]
        (fs/create-dirs dir)
        (spit (str dir "/index.html")
              (render post-template
                      {:title title
                       :date display-date
                       :content html
                       :draft-tag (draft-tag draft?)}))))
    ;; rss (published posts only)
    (let [published (remove :draft? sorted)]
      (spit (str blog-dir "/rss.xml")
            (render rss-template
                    {:items (str/join "\n    "
                              (map (fn [{:keys [title slug date html]}]
                                     (render rss-item-template
                                             {:title title :slug slug
                                              :pub-date (->rfc822 date)
                                              :content html}))
                                   published))})))
    (println (str "Built " (count posts) " posts"
                  (when (pos? n-drafts)
                    (str " (" n-drafts " drafts)"))))))

;; -- New draft --

(defn slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9\s-]" "")
      (str/trim)
      (str/replace #"\s+" "-")
      (str/replace #"-+" "-")))

(defn new-draft! [title]
  (when (str/blank? title)
    (println "Usage: bb blog.clj new \"Post Title\"")
    (System/exit 1))
  (fs/create-dirs drafts-dir)
  (let [slug (slugify title)
        file (str drafts-dir "/" slug ".md")
        today (str (java.time.LocalDate/now))]
    (when (fs/exists? file)
      (println (str "Already exists: " file))
      (System/exit 1))
    (spit file (str "---\ntitle: " title "\ndate: " today "\n---\n\n"))
    (println (str "Created " file))))

;; -- Serve --

(defn any-modified-since? [dirs ts]
  (some (fn [dir]
          (when (fs/exists? dir)
            (some #(> (.toMillis (fs/last-modified-time %)) ts)
                  (fs/glob dir "**"))))
        dirs))

(defn serve! []
  (build! true)
  (let [watch-dirs [posts-dir drafts-dir images-dir templates-dir]]
    (future
      (println "Watching for changes...")
      (loop [ts (System/currentTimeMillis)]
        (Thread/sleep 500)
        (let [changed? (any-modified-since? watch-dirs ts)]
          (when changed?
            (println (str "[" (java.time.LocalTime/now) "] Rebuilding..."))
            (build! true))
          (recur (if changed? (System/currentTimeMillis) ts))))))
  (println (str "Serving at http://localhost:" port))
  (server/exec {:port port :dir "."}))

;; -- CLI --

(let [[cmd & args] *command-line-args*]
  (case cmd
    "build" (build! false)
    "serve" (serve!)
    "new" (new-draft! (first args))
    (println "Usage: bb blog.clj [build|serve|new \"title\"]")))

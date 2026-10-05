#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[markdown.core :as md]
         '[babashka.http-server :as server])

;; -- Config --

(def blog-dir "blog")
(def posts-dir (str blog-dir "/posts"))
(def drafts-dir (str blog-dir "/drafts"))
(def unlisted-dir (str blog-dir "/unlisted"))
(def images-dir (str blog-dir "/images"))
(def templates-dir (str blog-dir "/templates"))
(def static-dir "static")
(def pub-dir "pub")
(def port 1313)

;; -- Templates --

(defn template [filename]
  (slurp (str templates-dir "/" filename)))

(defn render [tmpl vars]
  (reduce-kv (fn [html k v]
               (str/replace html (str "{{" (name k) "}}") (or v "")))
             tmpl vars))

;; -- Parsing --

(def ^:private iso-fmt (java.text.SimpleDateFormat. "yyyy-MM-dd"))

(defn ->rfc822 [date-str]
  (let [ld (java.time.LocalDate/parse date-str)
        zdt (.atStartOfDay ld (java.time.ZoneOffset/UTC))]
    (.format zdt java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(def ^:private display-date-fmt
  (java.time.format.DateTimeFormatter/ofPattern "MMMM d, yyyy"))

(defn ->display-date [date-str]
  (.format (java.time.LocalDate/parse date-str) display-date-fmt))

(def site-url "https://kitallis.in")

(defn- attr-escape [s]
  (-> s (str/replace "\"" "&quot;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- first-paragraph
  "Plain text of the first non-empty <p>, trimmed to fit a link preview."
  [html]
  (some->> (re-seq #"(?s)<p>(.*?)</p>" html)
           (map #(-> (second %) (str/replace #"<[^>]+>" "") (str/replace #"\s+" " ") str/trim))
           (remove str/blank?)
           first
           (#(if (> (count %) 200) (str (str/trim (subs % 0 197)) "…") %))))

(defn- first-image
  "Link previews (Slack, X, iMessage) don't render SVG, so only raster images qualify."
  [html]
  (some->> (re-seq #"<img\s[^>]*src=\"([^\"]+)\"" html)
           (map second)
           (filter #(re-find #"(?i)\.(png|jpe?g|gif|webp)$" %))
           first
           (#(if (str/starts-with? % "http") % (str site-url %)))))

(defn parse-post [file draft?]
  (let [content (slurp (str file))
        filename (str (fs/file-name file))
        {:keys [metadata html]} (md/md-to-html-string-with-meta content :footnotes? true :heading-anchors true)
        has-date? (re-find #"^\d{4}-\d{2}-\d{2}-" filename)
        filename-date (when has-date? (subs filename 0 10))
        date (if-let [d (:date metadata)]
               (.format iso-fmt d)
               (or filename-date (str (java.time.LocalDate/now))))
        name-part (if has-date?
                    (subs filename 11 (- (count filename) 3))
                    (subs filename 0 (- (count filename) 3)))]
    {:title (or (:title metadata) name-part)
     :date date
     :display-date (->display-date date)
     :slug name-part
     :name-part name-part
     :draft? draft?
     :description (or (:description metadata) (first-paragraph html) (:title metadata))
     :image (first-image html)
     :html html}))

;; -- Inline SVG --

(defn- add-viewbox
  "Monodraw & co. emit width/height but no viewBox, which blocks responsive scaling."
  [svg]
  (let [tag (re-find #"(?s)<svg\b[^>]*>" svg)
        w (when tag (second (re-find #"\swidth=\"(\d+(?:\.\d+)?)(?:px)?\"" tag)))
        h (when tag (second (re-find #"\sheight=\"(\d+(?:\.\d+)?)(?:px)?\"" tag)))]
    (if (and w h (not (re-find #"viewBox=" tag)))
      (str/replace-first svg #"<svg\b" (str "<svg viewBox=\"0 0 " w " " h "\""))
      svg)))

(defn- namespace-ids
  "Prefix ids so several inlined SVGs (e.g. Vega's clip0, clip1…) can't collide in one page."
  [svg prefix]
  (-> svg
      (str/replace #"(\s)id=\"([^\"]+)\"" (fn [[_ ws id]] (str ws "id=\"" prefix "-" id "\"")))
      (str/replace #"url\(#([^)]+)\)" (fn [[_ id]] (str "url(#" prefix "-" id ")")))
      (str/replace #"href=\"#([^\"]+)\"" (fn [[_ id]] (str "href=\"#" prefix "-" id "\"")))))

(defn- add-svg-title
  "A leading <title> is the SVG's hover tooltip; the alt replaces any the file already has."
  [svg alt]
  (if (str/blank? alt)
    svg
    (str/replace-first svg #"(?s)(<svg\b[^>]*>)(\s*<title>.*?</title>)?"
                       (fn [[_ open]] (str open "<title>" alt "</title>")))))

(defn- svg-file->markup
  "mono? opts into recolouring the SVG with the text colour (for black-only diagrams)."
  [src alt mono?]
  (let [path (str/replace src #"^/" "")]
    (when (and (str/ends-with? (str/lower-case src) ".svg")
               (fs/exists? path))
      (-> (slurp path)
          (str/replace #"(?s)^\s*<\?xml.*?\?>\s*" "")
          (str/replace #"(?s)^\s*<!DOCTYPE.*?>\s*" "")
          (add-viewbox)
          (namespace-ids (fs/strip-ext (fs/file-name path)))
          (add-svg-title alt)
          (str/replace-first #"<svg\b"
                             (str "<svg class=\"inline-svg" (when mono? " mono") "\" "
                                  (if (str/blank? alt)
                                    "aria-hidden=\"true\""
                                    (str "role=\"img\" aria-label=\""
                                         (str/replace alt "\"" "&quot;") "\""))))))))

(defn add-heading-anchors
  "Append a click-to-link anchor to each h2/h3 that markdown-clj gave an id."
  [html]
  (str/replace html
               #"(?s)<(h[23]) id=\"([^\"]+)\">(.*?)</\1>"
               (fn [[_ tag id inner]]
                 (str "<" tag " id=\"" id "\">" inner
                      "<a class=\"anchor\" href=\"#" id "\" aria-label=\"Link to this section\">#</a>"
                      "</" tag ">"))))

(defn- alt-as-title
  "Browsers only show title on hover, so reuse the alt rather than writing it twice."
  [tag alt]
  (if (or (str/blank? alt) (re-find #"\stitle=\"" tag))
    tag
    (str/replace-first tag #"<img\b" (str "<img title=\"" alt "\""))))

(defn inline-svgs
  "Replace <img src=\"....svg\"> with the file's markup, so it can be styled by CSS.
  A markdown title of \"mono\" — ![alt](x.svg \"mono\") — recolours it with the text colour.
  The alt doubles as the hover tooltip for both inlined SVGs and plain images."
  [html]
  (str/replace html
               #"<img\s[^>]*>"
               (fn [tag]
                 (let [src (second (re-find #"src=\"([^\"]+)\"" tag))
                       alt (or (second (re-find #"alt=\"([^\"]*)\"" tag)) "")
                       mono? (= "mono" (second (re-find #"title=\"([^\"]*)\"" tag)))]
                   (or (some-> src (svg-file->markup alt mono?))
                       (alt-as-title tag alt))))))

;; -- Build --

(defn load-posts [include-drafts?]
  (let [posts (when (fs/exists? posts-dir)
                (->> (fs/glob posts-dir "*.md")
                     (mapv #(parse-post % false))))
        drafts (when (and include-drafts? (fs/exists? drafts-dir))
                 (->> (fs/glob drafts-dir "*.md")
                      (mapv #(parse-post % true))))]
    (into (vec posts) drafts)))

(defn load-unlisted []
  (when (fs/exists? unlisted-dir)
    (->> (fs/glob unlisted-dir "*.md")
         (mapv #(assoc (parse-post % false) :unlisted? true)))))

(defn post-tag [{:keys [draft? unlisted?]}]
  (cond draft? (template "draft-tag.html")
        unlisted? (template "unlisted-tag.html")
        :else ""))

(defn post-link [{:keys [title display-date slug] :as post}]
  (render (template "post-link.html")
          {:slug slug :date display-date :title title :draft-tag (post-tag post)}))

(defn clean! []
  (when (fs/exists? pub-dir)
    (fs/delete-tree pub-dir)))

(defn copy-static!
  "static/ ships as-is at the site root; post images keep their /blog/images/ urls."
  []
  (doseq [child (fs/list-dir static-dir)]
    (let [dest (str pub-dir "/" (fs/file-name child))]
      (if (fs/directory? child)
        (fs/copy-tree child dest)
        (fs/copy child dest))))
  (fs/create-dirs (str pub-dir "/" blog-dir))
  (fs/copy-tree images-dir (str pub-dir "/" images-dir)))

(defn build! [include-drafts?]
  (clean!)
  (fs/create-dirs pub-dir)
  (let [posts (load-posts include-drafts?)
        unlisted (or (load-unlisted) [])
        sorted (sort-by :date #(compare %2 %1) posts)
        n-drafts (count (filter :draft? posts))
        n-unlisted (count unlisted)]
    ;; index (root page) — unlisted posts intentionally excluded
    (spit (str pub-dir "/index.html")
          (inline-svgs
           (render (template "index.html")
                   {:posts (str/join "\n      " (map post-link sorted))})))
    ;; posts + unlisted (both get a page, unlisted just aren't linked)
    (doseq [{:keys [slug title display-date description image html] :as post} (concat posts unlisted)]
      (let [dir (str pub-dir "/p/" slug)]
        (fs/create-dirs dir)
        (spit (str dir "/index.html")
              (inline-svgs
               (render (template "post.html")
                       {:title title
                        :description (attr-escape description)
                        :image (or image (str site-url "/hero/IMG_0002.jpg"))
                        :twitter-card (if image "summary_large_image" "summary")
                        :date display-date
                        :slug slug
                        :content (add-heading-anchors html)
                        :draft-tag (post-tag post)})))))
    ;; rss (published posts only, unlisted excluded)
    (let [published (remove :draft? sorted)
          rss (render (template "rss.xml")
                      {:items (str/join "\n    "
                                (map (fn [{:keys [title slug date html]}]
                                       (render (template "rss-item.xml")
                                               {:title title :slug slug
                                                :pub-date (->rfc822 date)
                                                :content html}))
                                     published))})]
      (spit (str pub-dir "/rss.xml") rss)
      (fs/create-dirs (str pub-dir "/rss"))
      (spit (str pub-dir "/rss/index.xml") rss))
    ;; subscribe page
    (fs/create-dirs (str pub-dir "/subscribe"))
    (spit (str pub-dir "/subscribe/index.html") (inline-svgs (template "subscribe.html")))
    (copy-static!)
    (println (str "Built " (count posts) " posts"
                  (when (pos? n-drafts) (str " (" n-drafts " drafts)"))
                  (when (pos? n-unlisted) (str " (" n-unlisted " unlisted)"))))))

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
    (spit file (render (template "frontmatter.md") {:title title :date today}))
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
  (let [watch-dirs [posts-dir drafts-dir unlisted-dir images-dir templates-dir static-dir]]
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
  (server/exec {:port port :dir pub-dir}))

;; -- CLI --

(let [[cmd & args] *command-line-args*]
  (case cmd
    "build" (build! false)
    "serve" (serve!)
    "new" (new-draft! (first args))
    (println "Usage: bb blog.clj [build|serve|new \"title\"]")))

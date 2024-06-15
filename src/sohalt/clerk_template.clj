(ns sohalt.clerk-template
  (:require [clojure.string :as str]
            [babashka.http-client :as curl]
            [cheshire.core :as cheshire]))

(def unexceptional-statuses
  #{200 201 202 203 204 205 206 207 300 301 302 303 304 307})

(def github-user (or (System/getenv "NEIL_GITHUB_USER")
                     (System/getenv "BABASHKA_NEIL_DEV_GITHUB_USER")))
(def github-token (or (System/getenv "NEIL_GITHUB_TOKEN")
                      (System/getenv "BABASHKA_NEIL_DEV_GITHUB_TOKEN")))

(def curl-opts
  (merge {:throw false}
         (when (and github-user github-token)
           {:basic-auth [github-user github-token]})))

(defn curl-get-json
  ([url] (curl-get-json url nil))
  ([url opts]
   (let [response    (curl/get url (merge curl-opts opts))
         parsed-body (try (-> response :body (cheshire/parse-string true))
                          (catch Exception e
                            (binding [*out* *err*]
                              (println "Unable to parse body as JSON:")
                              (println (ex-message e))
                              (println (-> response :body)))
                            nil #_(throw e)))]
     (cond
       (and (= 403 (:status response))
            (str/includes? url "api.github")
            (str/includes? (:message parsed-body) "rate limit"))
       (binding [*out* *err*]
         (println "You've hit the github rate-limit (60 reqs/hr).
  You can set an API Token to increase the limit.
  See neil's README for details.")
         nil #_(System/exit 1))

       (contains? unexceptional-statuses (:status response))
       parsed-body
       (= 404 (:status response))
       nil
       :else
       (binding [*out* *err*]
         (println
          (or (not-empty (:body response))
              (str url " request returned status code: " (:status response))))
         nil)))))

(defn default-branch [lib]
  (get (curl-get-json (format "https://api.github.com/repos/%s/%s"
                              (namespace lib) (name lib)))
       :default_branch))

(defn clean-github-lib [lib]
  (let [lib (str/replace lib "com.github." "")
        lib (str/replace lib "io.github." "")
        lib (symbol lib)]
    lib))

(defn latest-github-sha [lib]
  (let [lib (clean-github-lib lib)
        branch (default-branch lib)]
    (get (curl-get-json (format "https://api.github.com/repos/%s/%s/commits/%s"
                                (namespace lib) (name lib) branch))
         :sha)))

(defn data-fn
  "Example data-fn handler.

  Result is merged onto existing options data."
  [data]
  ;; returning nil means no changes to options data
  (println "data-fn returning nil")
  {:clerk-sha (latest-github-sha "io.github.nextjournal/clerk")})

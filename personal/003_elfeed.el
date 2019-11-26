;; elfeed
;; data is stored in ~/.elfeed
(setq elfeed-feeds
      '(("https://www.reddit.com/r/programming.rss" programming)
        ("https://www.reddit.com/r/emacs.rss" emacs)
        ("https://www.reddit.com/r/golang.rss" golang)
        ("https://www.reddit.com/r/java.rss" java)
        ("https://www.reddit.com/r/scala.rss" scala)
        ("https://www.reddit.com/r/javascript.rss" javascript)
        ("https://www.reddit.com/r/typescript.rss" typescript)
        ("https://www.reddit.com/r/aws.rss" aws)
        ("https://www.reddit.com/r/googlecloud.rss" googlecloud)
        ("https://www.reddit.com/r/devops.rss" devops)
        ("https://www.reddit.com/r/azure.rss" azure)
        ("https://www.reddit.com/r/kubernetes.rss" kubernetes)
        ("https://www.reddit.com/r/lisp.rss" lisp)
        ("https://www.reddit.com/r/clojure.rss" clojure)
        ("https://www.reddit.com/r/python.rss" python)
        ("https://www.reddit.com/r/kotlin.rss" kotlin)
        ("https://news.ycombinator.com/rss" hacker)
        ("https://www.heise.de/developer/rss/news-atom.xml" heise)))

(setq-default elfeed-search-filter "@2-days-ago +unread")

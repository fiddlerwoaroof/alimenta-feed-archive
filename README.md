Dump a collection of feeds to a directory.

To use, clone alimenta ( https://github.com/fiddlerwoaroof/alimenta ), my fork of chronicity (
https://github.com/fiddlerwoaroof/chronicity ) and my utility library (
https://github.com/fiddlerwoaroof/fwoar.lisputils ) to your local-projects directory and then use quicklisp to
load alimenta-feed-archive.  Then, execute something like:

```
(alimenta.feed-archive::init-feeds '("http://example.com/feed") "/home/me/my_feeds")
```

Next create the directory specified in the last argument and copy the contents of the archive-root directory
to it.  Then run (in lisp) `(alimenta.feed-archive::archive-feeds)` and it should populate that directory with
your feeds.  Then, if that directory is accessible via a web server, you should be able to navigate to it and
browse through your feeds.


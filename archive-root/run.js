root = new Vue({
  el: '#container',
  data: {
    "pull_time": null,
    "feed_urls": [],
    feeds: {
      feeds: []
    },

    current_feed: {
      title: null,
      fetch_url: null,
      link: null,
      description: null,
      items: [],
      base_path: null,
    },

    current_item: {
      title: null,
      date: null,
      author: null,
      id: null,
      link: null,
      content: null,
    },
  },

  computed: {
    item_content() {
      if (this.current_item.content !== null) {
        return DOMPurify.sanitize(this.current_item.content);
      }
    },
  },

  methods: {
    sanitize(html) {
      return DOMPurify.sanitize(html, {
        FORBID_TAG: ['style'],
        FORBID_ATTR: ['style'],
      });
    },

    get_feed: function (path) {
      window.fetch(path+'index.json').then((resp) => resp.json())
      .then((data) => {
        var result = Object.assign({}, data);
        result.fetch_url = data['fetch-url'];
        result.base_path = path;
        window.history.pushState({
          'current_feed': result
        }, "", window.location.pathname);
        Object.assign(root.current_feed, result);
      });
    },

    get_item(path) {
      window.fetch(this.current_feed.base_path + path).then((resp) => resp.json())
      .then((data) => {
        window.history.pushState({
          'current_feed': root.current_feed,
          'current_item': data
        }, "", window.location.pathname);
        Object.assign(this.current_item, data);
      });
    }

  }
});

window.fetch(baseUrl+'/index.json').then((resp) => resp.json())
.then(function (data) {
  root.pull_time = data['pull-time'];
  root.feed_urls = data['feed-urls'];
  root.feeds = data.feeds;
});

window.onpopstate = function (ev) {
  console.log(ev);
  var current_feed = ev.state.current_feed, current_item = ev.state.current_item;

  Object.assign(root.current_feed, current_feed);

  if (current_item !== undefined) {
    Object.assign(root.current_item, current_item);
  }
};

document.addEventListener('DOMContentLoaded', function (ev) {
  if (window.history.state !== null) {
    Object.assign(root.current_feed, window.history.state.current_feed);
    if (window.history.state.current_item !== undefined) {
      Object.assign(root.current_item, window.history.state.current_item);
    }
  }
});

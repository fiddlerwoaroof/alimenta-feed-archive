root = new Vue({
    el: '#container',
    data: {
	"pull_time": null,
	"feed_urls": [],
	feeds: {
	    feeds: []
	},

	current_feed: {
	    metadata: {
		description: null,
		fetch_url: null,
		link: null,
		title: null,
	    },
	    base_path: null,
	    items: [],
	},

	current_item: {
	    title: null,
	    date: null,
	    author: null,
	    id: null,
	    link: null,
	    content: null,
	},

	feed_item_counts: {},
    },

    computed: {
	item_content() {
	    let result = null;
	    if (this.current_item.content !== null) {
		result = DOMPurify.sanitize(this.current_item.content);
	    }
	    return result;
	},
    },

    methods: {
	sanitize(html) {
	    return DOMPurify.sanitize(html, {
		FORBID_TAG: ['style'],
		FORBID_ATTR: ['style'],
	    });
	},

	get_remote_feed: function (path) {
	    var promise = new Promise((resolve, reject) => {
		window.fetch(path+'index.json').then((resp) => resp.json())
		    .then((data) => {
			var result = Object.assign({}, data);
			result.fetch_url = data['fetch-url'];
			result.base_path = path;
			window.history.pushState({
			    'current_feed': result
			}, "", window.location.pathname);
			resolve(result);
			return promise;
		    }, reject.bind(promise));
	    });
	    return promise;
	},

	get_feed: function (path) {
	    this.get_remote_feed(path)
		.then((result) =>
		      Object.assign(root.current_feed, result));
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
	},

	has_items(feed) {
	    var count = this.feed_item_counts[feed.path];
	    return count === undefined || (count > 0);
	},

    },

    ready() {
	window.fetch(baseUrl+'/index.json').then((resp) => resp.json())
	    .then(function (data) {
		root.pull_time = data['pull-time'];
		root.feed_urls = data['feed-urls'];
		root.feeds = data.feeds;
		return data;
	    })
	    .then((data) => {
		root.feeds.forEach((feed) => {
		    // console.log(feed);
		    this.get_remote_feed(feed.path)
			.then((feed_index) => Vue.set(this.feed_item_counts, feed.path, feed_index.items.length));
		});
	    });

    },

});

window.onpopstate = function (ev) {
    // console.log(ev);
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

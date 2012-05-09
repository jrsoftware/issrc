/*
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  $jrsoftware: ishelp/Staging/topic.js,v 1.7 2010/04/25 02:48:24 jr Exp $

  JavaScript code used by topic_*.htm
*/

window.onload = topic_loaded;

var redirectBox = null;
var topicRedirectURL;
var curHighlightedAnchor = null;
var lastHash = "";

function is_href_local_anchor(path)
{
	// Returns true if an A element's HREF points to an anchor on the current page.

	if (path.charAt(0) == "#") {
		return true;
	}

	// On IE (6), # HREFs are prepended with a full path when read via DOM.
	// Check if 'path' includes a hash, and if the part before the hash
	// matches the current window.location.

	var re = /^([^#]+)(#.*)?$/;
	var pathMatches = path.match(re);
	if (pathMatches && pathMatches[2] !== undefined && pathMatches[2] != "") {
		var curLocationMatches = window.location.href.match(re);
		if (curLocationMatches && curLocationMatches[1] == pathMatches[1]) {
			return true;
		}
	}

	return false;
}

function get_viewport_element()
{
	// On IE 6 in Standards mode & Firefox 1.5, properties like
	// scrollTop and clientHeight are set on document.documentElement.
	// On IE 5, they're on document.body; the ones on documentElement
	// are zero.

	if (document.documentElement.clientHeight) {
		return document.documentElement;
	} else {
		return document.body;
	}
}

function create_redirect_box()
{
	redirectBox = document.createElement("div");
	redirectBox.id = "redirectbox";
	redirectBox.innerHTML = "Loading contents...";
	redirectBox.style.visibility = "hidden";
	document.body.appendChild(redirectBox);

	var viewport = get_viewport_element();
	redirectBox.style.left = Math.max(0, Math.floor(viewport.scrollLeft + (viewport.clientWidth - redirectBox.offsetWidth) / 2)) + "px";
	redirectBox.style.top = Math.max(0, Math.floor(viewport.scrollTop + (viewport.clientHeight - redirectBox.offsetHeight) / 2)) + "px";
	redirectBox.style.visibility = "";
}

function topic_loaded()
{
	document.body.onclick = topic_element_clicked;
	update_anchor_highlight();

	var matches;

	if (window == window.top &&
	    window.location.protocol == "http:" &&
	    window.location.hostname == "www.jrsoftware.org" &&
	    (matches = window.location.pathname.match(/^(\/.+\/)topic_([a-z0-9_\-]+)\.htm$/)) &&
	    window.location.hash != "#noredir") {

		topicRedirectURL = matches[1] + "index.php?topic=" + matches[2];

		if ((matches = window.location.hash.match(/^#([a-zA-Z0-9_\-.]+)$/))) {
			topicRedirectURL += "&anchor=" + matches[1];
		}

		create_redirect_box();
		window.setTimeout(topic_redirect, 1500);
	}

	// HTML Help: If an Index entry is clicked and only the hash changes,
	// we don't receive any notification by default.
	if (window.location.protocol == "mk:") {
		if ("onhashchange" in window) {
			// IE 8+ supports the onhashchange event.
			window.onhashchange = update_anchor_highlight;
		} else {
			// On older IE versions, we have to poll.
			window.setInterval(update_anchor_highlight, 300);
		}
	}
}

function topic_redirect()
{
	document.body.removeChild(redirectBox);
	redirectBox = null;

	window.location.href = topicRedirectURL;
}

function set_anchor_highlight(newAnchor)
{
	if (curHighlightedAnchor != newAnchor) {
		if (curHighlightedAnchor) curHighlightedAnchor.className = "";
		curHighlightedAnchor = newAnchor;
		if (curHighlightedAnchor) curHighlightedAnchor.className = "highlighted";
	}
}

function update_anchor_highlight()
{
	var anchorName = "";
	var hash = window.location.hash;
	if (hash === lastHash) {
		// This function can be called from setInterval,
		// so exit quickly if the hash hasn't changed.
		return;
	}
	lastHash = hash;
	if (hash.charAt(0) == "#") {
		anchorName = hash.substr(1);
	}

	var newAnchor = null;
	if (anchorName != "") {
		var anchors = document.getElementsByTagName("a");
		var i;
		for (i = 0; i < anchors.length; i++) {
			if (anchors[i].getAttribute("name") === anchorName) {
				newAnchor = anchors[i];
				break;
			}
		}
	}
	set_anchor_highlight(newAnchor);
}

function set_href_and_highlight_anchor(href)
{
	// At the moment an onclick event is fired, window.location.hash hasn't
	// been updated yet. This function synchronously updates window.location,
	// then moves the highlight to the new hash's anchor.

	// Clear current highlight first, so user doesn't see it scrolling.
	lastHash = "";
	set_anchor_highlight(null);
	window.location.href = href;
	update_anchor_highlight();
}

function topic_element_clicked(evt)
{
	var element = evt ? evt.target : event.srcElement;
	if (element.tagName.toLowerCase() == "a") {
		var href = element.getAttribute("href");
		if (href != null && is_href_local_anchor(href)) {
			set_href_and_highlight_anchor(href);
		}
	}
}

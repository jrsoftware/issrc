/*
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  JavaScript code used by topic_*.htm
*/

window.onload = topic_loaded;

var redirectBox = null;
var topicRedirectURL;

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
	var matches;

	if (window == window.top &&
	    (window.location.protocol == "http:" || window.location.protocol == "https:") &&
	    (window.location.hostname == "jrsoftware.org" || window.location.hostname == "www.jrsoftware.org") &&
	    (matches = window.location.pathname.match(/^(\/.+\/)topic_([a-z0-9_\-]+)\.htm$/)) &&
	    window.location.hash != "#noredir") {

		topicRedirectURL = matches[1] + "index.php?topic=" + matches[2];

		if ((matches = window.location.hash.match(/^#([a-zA-Z0-9_\-.]+)$/))) {
			topicRedirectURL += "&anchor=" + matches[1];
		}

		create_redirect_box();
		window.setTimeout(topic_redirect, 1500);
	}

	// Inform parent frame (index.htm) that the topic changed
	if (window.parent) {
		if ((matches = window.location.pathname.match(/\/topic_([a-z0-9_\-]+)\.htm$/))) {
			window.parent.postMessage("ishelp_topic_loaded:" + matches[1], "*");
		}
	}
}

function topic_redirect()
{
	document.body.removeChild(redirectBox);
	redirectBox = null;

	window.location.href = topicRedirectURL;
}

/*
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  JavaScript code used by contents.htm
*/

function get_absolute_top(obj)
{
	var y = obj.offsetTop;
	while ((obj = obj.offsetParent)) {
		y += obj.offsetTop;
	}
	return y;
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

function is_element_displayed(element)
{
	do {
		if (element.style.display == "none") return false;
	} while (element != document.body && (element = element.parentNode));

	return true;
}

function ensure_elements_visible(elementTop, elementBottom)
{
	if (!is_element_displayed(elementTop) || !is_element_displayed(elementBottom)) {
		return;
	}

	var yTop = get_absolute_top(elementTop);
	var yBottom = get_absolute_top(elementBottom) + elementBottom.offsetHeight;

	// Subtract 1 from yTop so that if elementTop contains a link with text that starts at
	// exactly yTop, the link's focus rectangle won't get chopped off on Firefox (3.x),
	// where focus rectangles are inflated by 1px (unlike IE).
	// (Adding 1 to yBottom isn't necessary since our TDs have 1px of bottom padding.)
	if (yTop > 0) yTop--;

	// Make yTop and yBottom relative to the top of the visible client area
	var viewportScrollTop = get_viewport_element().scrollTop;
	yTop -= viewportScrollTop;
	yBottom -= viewportScrollTop;

	var clientHeight = get_viewport_element().clientHeight;

	if (yTop < 0) {
		// Scroll up to make the top of elementTop visible
		window.scrollBy(0, yTop);
	} else if (yBottom > clientHeight) {
		// How far do we have to scroll down for elementBottom to be entirely visible?
		var delta = yBottom - clientHeight;
		// Don't allow any part of elementTop to be scrolled off the top
		if (delta > yTop) delta = yTop;
		if (delta > 0) window.scrollBy(0, delta);
	}
}

function toggle_node(id)
{
	var objContent = document.getElementById("nodecontent_" + id);
	var expanding = (objContent.style.display == "none");
	objContent.style.display = expanding ? "" : "none";

	document.getElementById("nodeimg_" + id).src =
		expanding ? "images/contentsheadopen.svg" : "images/contentsheadclosed.svg";

	if (expanding) {
		// Scroll expanded items into view. This is similar to calling scrollIntoView() but
		// doesn't do any scrolling if the items are already fully visible.

		var objCaption = document.getElementById("nodecaption_" + id);
		ensure_elements_visible(objCaption, objContent);
	}
}

function init_contents(toggleNode)
{
	var i;
	if (toggleNode == 0) {
		for (i = 1; document.getElementById("nodecontent_" + i) != null; i++) {
			toggle_node(i);
		}
	} else {
		toggle_node(toggleNode);
	}

	var elements = document.getElementById("tabbody-contents").getElementsByTagName("a");
	for (i = 0; i < elements.length; i++) {
		elements[i].onfocus = node_focused;
		elements[i].onblur = node_blurred;
	}
}

var curSelectedNode = null;
var curFocusedNode = null;

function update_selected_node_class()
{
	if (curSelectedNode) {
		var newClass = (curFocusedNode == curSelectedNode) ? "focusedlink" : "selectedlink";
		if (curSelectedNode.className != newClass) {
			curSelectedNode.className = newClass;
		}
	}
}

function set_selected_node(newSel)
{
	if (curSelectedNode == newSel) return;

	if (curSelectedNode) {
		curSelectedNode.className = "";
	}
	curSelectedNode = newSel;
	if (curSelectedNode) {
		update_selected_node_class();

		// Expand parent nodes (may scroll)
		var p = curSelectedNode;
		while ((p = p.parentNode) && p.id != "tabbody-contents") {
			if (p.id && p.id.indexOf("nodecontent_") == 0 && p.style.display == "none") {
				toggle_node(p.id.substring(12));
			}
		}

		// Then scroll the node's parent TR into view
		p = curSelectedNode.parentNode.parentNode;
		ensure_elements_visible(p, p);
	}
}

function node_focused(evt)
{
	curFocusedNode = evt ? evt.target : event.srcElement;
	if (curFocusedNode == curSelectedNode) {
		update_selected_node_class();
	} else {
		set_selected_node(curFocusedNode);
	}
}

function node_blurred(evt)
{
	curFocusedNode = null;
	update_selected_node_class();
}

var topic_name_regexp = /(?:^|[/\\])topic_([a-z0-9_\-]+)\.htm$/;

function topic_name_from_path(path)
{
	var matches = path.match(topic_name_regexp);
	return matches ? matches[1] : "";
}

function sync_contents()
{
	var bodyFrame = window.parent.frames["bodyframe"];
	if (!bodyFrame) return;

	var bodyTopic = topic_name_from_path(bodyFrame.window.location.pathname);
	if (bodyTopic == "") return;

	// If the currently selected node already points to bodyTopic, just return.
	// This check is needed to keep the selection from jumping to "[Run] section"
	// when "[UninstallRun] section" is clicked (both have the same target topic).
	if (curSelectedNode && topic_name_from_path(curSelectedNode.getAttribute("href")) == bodyTopic) {
		return;
	}

	var elements = document.getElementById("tabbody-contents").getElementsByTagName("a");
	var i;
	for (i = 0; i < elements.length; i++) {
		if (topic_name_from_path(elements[i].getAttribute("href")) == bodyTopic) {
			if (curSelectedNode != elements[i]) {
				// If we're changing the selection while a node is currently
				// focused -- which can happen if Back is pressed after
				// clicking/selecting a node -- we need to move the focus.
				// Otherwise, the focus rectangle would stay where it is,
				// while the highlight moved to a different node.

				if (curFocusedNode) elements[i].focus();
				set_selected_node(elements[i]);
			}
			break;
		}
	}
}

function select_tab(newTab)
{
	var tabs = ["contents", "index"];

	var i;
	for (i = 0; i < tabs.length; i++) {
		if (tabs[i] != newTab) {
			document.getElementById("tab-" + tabs[i]).className = "unselectedtab";
			document.getElementById("tabbody-" + tabs[i]).style.display = "none";
		}
	}

	document.getElementById("tab-" + newTab).className = "selectedtab";
	document.getElementById("tabbody-" + newTab).style.display = "";

	if (newTab == "index") init_index_tab();
}

var indexTabInited = false;

function init_index_tab()
{
	if (indexTabInited) return;
	indexTabInited = true;

	var script = document.createElement("script");
	script.src = "contentsindex.js";
	script.type = "text/javascript";
	document.getElementsByTagName("head")[0].appendChild(script);

	// contentsindex.js calls init_index_tab_elements()
}

function init_index_tab_elements()
{
	var html = "ERROR!";

	if (contentsIndexData) {
		var len = contentsIndexData.length;
		var htmlArray = new Array(len);
		var i, matches;
		var re = /^([^#:]+)(#[^:]+)?:(.+)$/

		for (i = 0; i < len; i++) {
			matches = contentsIndexData[i].match(re);
			if (!matches) break;
			htmlArray[i] = '<a href="topic_' + matches[1] + ".htm" +
				((matches[2] !== undefined) ? matches[2] : "") +
				'" target="bodyframe">' + matches[3] + "</a><br />";
		}

		// Note: On IE6, joining an array is ~5x faster than using "html +=" to build a long string
		if (i == len) {   // were all processed?
			html = htmlArray.join("");
		}
	}

	var indexBody = document.getElementById("tabbody-index");
	indexBody.onclick = index_tab_element_clicked;
	indexBody.innerHTML = html;
}

function index_tab_element_clicked(evt)
{
	// If an index link is clicked and only the hash changes on bodyframe
	// (i.e. still same page), bodyframe doesn't receive any notification.
	// So we must manually tell it to update the highlight.

	var element = evt ? evt.target : event.srcElement;
	if (element.tagName.toLowerCase() == "a") {
		var href = element.getAttribute("href");
		if (href != null && href != "" && element.getAttribute("target") == "bodyframe") {
			var bodyFrame = window.parent.frames["bodyframe"];
			if (bodyFrame) {
				bodyFrame.set_href_and_highlight_anchor(href);
			}
		}
	}
}

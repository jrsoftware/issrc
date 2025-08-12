/*
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
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
		if (element.hidden) return false;
	} while ((element = element.parentElement));

	return true;
}

function ensure_elements_visible(elementTop, elementBottom)
{
	if (!is_element_displayed(elementTop) || !is_element_displayed(elementBottom)) {
		return;
	}

	const scrollerElement = document.getElementById("tabbody-contents");

	// Inflate by 2 pixels to ensure that focus rectangles are fully visible
	let yTop = get_absolute_top(elementTop) - 2;
	let yBottom = get_absolute_top(elementBottom) + elementBottom.offsetHeight + 2;

	// Make yTop and yBottom relative to the top of the visible client area
	const scrollerTop = get_absolute_top(scrollerElement) + scrollerElement.scrollTop;
	yTop -= scrollerTop;
	yBottom -= scrollerTop;

	if (yTop < 0) {
		// Scroll up to make the top of elementTop visible
		scrollerElement.scrollBy(0, yTop);
	} else if (yBottom > scrollerElement.clientHeight) {
		// How far do we have to scroll down for elementBottom to be entirely visible?
		let delta = yBottom - scrollerElement.clientHeight;
		// Don't allow any part of elementTop to be scrolled off the top
		if (delta > yTop) delta = yTop;
		if (delta > 0) scrollerElement.scrollBy(0, delta);
	}
}

function toggle_node(id)
{
	const contentElement = document.getElementById("nodecontent_" + id);
	const itemElement = contentElement.parentElement;
	const linkElement = itemElement.querySelector(":scope > a");
	const imageElement = linkElement.querySelector(":scope > img");

	const expanding = !!contentElement.hidden;
	contentElement.hidden = !expanding;

	linkElement.setAttribute("aria-expanded", expanding);

	imageElement.src = expanding ? "images/contentsheadopen.svg" : "images/contentsheadclosed.svg";
	imageElement.alt = expanding ? "\u25BC " : "\u25B6 ";

	if (expanding) {
		// Scroll expanded items into view. This is similar to calling scrollIntoView() but
		// doesn't do any scrolling if the items are already fully visible.

		ensure_elements_visible(itemElement, itemElement);
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
}

var curSelectedNode = null;

function set_selected_node(newSel)
{
	if (curSelectedNode == newSel) return;

	if (curSelectedNode) {
		curSelectedNode.removeAttribute("aria-selected");
	}
	curSelectedNode = newSel;
	if (curSelectedNode) {
		curSelectedNode.setAttribute("aria-selected", true);

		// Expand parent nodes (may scroll)
		let p = curSelectedNode;
		while ((p = p.parentElement) && p.id !== "tabbody-contents") {
			if (p.id && p.id.startsWith("nodecontent_") && p.hidden) {
				toggle_node(p.id.substring(12));
			}
		}

		// Then scroll the node's A element into view
		ensure_elements_visible(curSelectedNode, curSelectedNode);

		// If the focus is currently inside the Contents tab panel (and not inside the
		// topic body frame), then ensure the new selected node is focused. This matters
		// when Back is clicked in the browser; we want the selection and the focus
		// rectangle to both move back to the previous node.
		if (document.getElementById("tabbody-contents").contains(document.activeElement)) {
			curSelectedNode.focus();
		}
	}
}

var topic_name_regexp = /(?:^|[/\\])topic_([a-z0-9_\-]+)\.htm$/;

function topic_name_from_path(path)
{
	var matches = path.match(topic_name_regexp);
	return matches ? matches[1] : "";
}

function sync_contents(bodyTopic)
{
	if (!bodyTopic) return;

	const elements = document.getElementById("tabbody-contents").getElementsByTagName("a");
	for (let i = 0; i < elements.length; i++) {
		if (topic_name_from_path(elements[i].getAttribute("href")) === bodyTopic) {
			set_selected_node(elements[i]);
			break;
		}
	}
}

function select_tab(newTab)
{
	const tabs = ["contents", "index"];

	for (let i = 0; i < tabs.length; i++) {
		if (tabs[i] != newTab) {
			document.getElementById("tab-" + tabs[i]).setAttribute("aria-selected", false);
			document.getElementById("tabbody-" + tabs[i]).hidden = true;
		}
	}

	document.getElementById("tab-" + newTab).setAttribute("aria-selected", true);
	document.getElementById("tabbody-" + newTab).hidden = false;

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

	document.getElementById("tabbody-index").innerHTML = html;
}

window.addEventListener("message", (event) => {
	//console.log("contents.js message received:", event.data);

	if (typeof event.data === "string" && event.data.startsWith("ishelp_sync_contents:")) {
		sync_contents(event.data.substring(21));
	}
});

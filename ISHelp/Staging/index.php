<?php

$topic = @$_GET['topic'];
if (!preg_match('/^[a-z0-9_\-]+\z/', $topic))
  $topic = 'whatisinnosetup';

$anchor = '#' . @$_GET['anchor'];
if (!preg_match('/^#[a-zA-Z0-9_\-.]+\z/', $anchor))
  $anchor = '';

$text = @file_get_contents('index.htm');
if (!$text) die('Error reading file');

$text = str_replace('topic_whatisinnosetup.htm', htmlspecialchars("topic_$topic.htm$anchor"), $text);
echo $text;

?>

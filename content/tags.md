---
title: Tags
header: Postagens por tags
---
<% tags = count_tags %>

<ul class="tag_box inline">
<% tags.each do |tag, num| %>
<li><a href="#<%= tag %>-ref"><%= tag %> <span class="badge"><%= num %></span></a></li>
<% end %>
</ul>

<% tags.each do |tag, num| %>
<h2 id="<%= tag %>-ref"><%= tag %></h2>
<ul class="list-group">
<% relevantItems = items_with_tag(tag)
   relevantItems.each do |item| %>
<li class="list-group-item"><a href="<%= @config[:base_url] + item.identifier.chop + '.html' %>"><%= item[:title] %></a></li>
<% end %>
</ul>
<% end %>

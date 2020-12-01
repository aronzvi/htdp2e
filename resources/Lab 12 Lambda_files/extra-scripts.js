AddOnLoad(function(){
  var tocsetElt = document.querySelector("div.tocset");
  if (!tocsetElt) { return ; } // Dunno why this is needed, but it seems to help
  var tocsetStr = tocsetElt.outerHTML.replace(new RegExp("tocview_\\d+", "g"), "topbar_$&");
  var newtocset = document.createElement("div");
  var navsettop = document.querySelector("div.navsettop");
  var container = document.createElement("div");
  container.classList.add("topbar", "navleft", "hidden");
  var btn = document.createElement("a");
  btn.href = "javascript:void(0);";
  btn.innerText = "Table of Contents";
  btn.addEventListener("click", function() { container.classList.toggle("hidden"); });
  navsettop.insertBefore(container, navsettop.firstChild);
  container.appendChild(btn); container.appendChild(newtocset);
  newtocset.outerHTML = tocsetStr;
});

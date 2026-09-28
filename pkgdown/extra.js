// The table of contents of the articles ("On this page") shows three levels of
// headings. bootstrap-toc, which pkgdown uses to build it, shows only two: the
// top level and the one below. This runs before the table of contents is built,
// when the page is ready.
(function () {
  if (!window.Toc) return;
  var helpers = window.Toc.helpers;
  var depth = 3;

  helpers.getHeadings = function ($scope, topLevel) {
    var tags = [];
    for (var i = 0; i < depth; i++) tags.push("h" + (topLevel + i));
    return helpers.findOrFilter($scope, tags.join(","));
  };

  // each heading goes in the list of the last heading of a higher level
  helpers.populateNav = function ($topList, topLevel, $headings) {
    var stack = [{ level: topLevel - 1, $item: null, $list: $topList }];
    $headings.each(function (_, heading) {
      var level = helpers.getNavLevel(heading);
      while (stack.length > 1 && stack[stack.length - 1].level >= level) stack.pop();
      var parent = stack[stack.length - 1];
      if (!parent.$list) parent.$list = helpers.createChildNavList(parent.$item);
      var $item = helpers.generateNavItem(heading);
      parent.$list.append($item);
      stack.push({ level: level, $item: $item, $list: null });
    });
  };
})();

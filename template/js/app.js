
;(function ($) {
	/* cloused local variables */
	var __drag = null;
	var __prev = null;
	var __wrap = null;
	var __diff = {x: 0, y: 0};

	$(document).bind("mousemove.box", function (e) {
		if (__wrap == null) {
			return;
		}
		__wrap.css({"left": e.pageX + __diff.x + "px", "top": e.pageY + __diff.y + "px"});
	});

	$(document).bind("mouseup.box", function (e) {
		if (__drag == null || __wrap == null) {
			return;
		}

		var drop = null;
		$(".dlg").each(function (i) {
			if (__drag.get(0) != this) {
				var tmp = $(this);
				var pos = tmp.position();
				var h = tmp.height();
				var w = tmp.width();
				if (e.pageX >= pos.left && e.pageX <= (pos.left + w) && e.pageY >= pos.top && e.pageY <= (pos.top + h)) {
					drop = tmp;
				}
			}
		});

		if (drop == null) {
			__prev.append(__drag);
		} else {
			var prnt = drop.parent();
			__prev.append(drop);
			prnt.append(__drag);
		}

		__wrap.remove();
		__prev = null;
		__wrap = null;
		__drag = null;
		__diff = {x: 0, y: 0};
	});

	var __createBox = function (container, title) {
        var cnt = container.html();
        container.html("");
        
        var title = container.attr("title");
        
        var title = $('<div class="t"></div>')
						.append('<div class="tl"></div>')
						.append($('<span></span>').html(title))
						.append('<div class="tr"></div>');

		var dlg = $('<div class="dlg"></div>')
			.append(title)
			.append($('<div class="c"></div>').html(cnt))
			.append($('<div class="b"></div>')
				.append('<div class="bl"></div>')
				.append('<div class="br"></div>'));
		container.append(dlg);

		title.bind("mousedown.box", {"dialog": dlg}, function (e) {
			if (__drag != null) {
				return;
			}

			var dialog = e.data.dialog;
			var pos = dialog.position();
			var w = dialog.width();
			var h = dialog.height();

			__prev = dialog.parent();
			__diff = {"x":  pos.left - e.pageX, "y": pos.top - e.pageY};
			__wrap = $("<div></div>")
				.css({"position": "absolute",
					"left": pos.left + "px",
					"top": pos.top + "px",
					"height": h + "px",
					"width": w + "px"})
				.append(dialog);
			$(document.body).append(__wrap);

			__drag = dialog;
		});

		return dlg;
	};

	$.fn.addbox = function () {
		return this.each(function (i) {
			var o = $(this);
			return __createBox(o);
		});
	};
})(jQuery);

(function ($) { 
    var __tabify = function (self, onChanged) {
        var selectedId = null;
        
        $(tabs).find("ul.tabs li").each(function (index, value) {
            var idx = index;
            var obj = $(value);
            var id = obj.attr("id");
            
            if (obj.hasClass("selected")) {
                selectedId = id;
                $("#" + id + "_page").css("display", "block");
                
                if ($.isFunction(onChanged)) {
                    onChanged(obj, idx);
                }
            }
            
            obj.click(function () {
                if (id != selectedId) {
                    $("#" + selectedId).removeClass("selected");
                    $("#" + selectedId + "_page").css("display", "none");
                }
                
                $("#" + id).addClass("selected");
                $("#" + id + "_page").css("display", "block");
                selectedId = id;
                
                if ($.isFunction(onChanged)) {
                    onChanged(obj, idx);
                }
            });
        });
        return self;
    }
    
    $.fn.tabify = function (onChanged) {
        return this.each (function () {
            return __tabify($(tabs), onChanged);
        });
    }
})(jQuery);

$(function () {
    // Add boxes on sensor page
    $("#c0").addbox();
    $("#c2").addbox();
    $("#c3").addbox();
    $("#c4").addbox();
    
    // Adds chart on
    var r = Raphael("plot", "100%", 350);
    r.g.txtattr.font = "12px 'Fontin Sans', Fontin-Sans, sans-serif";
    var lines = r.g.linechart(15, 15, 550, 320,
                        [[1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6]],
                        [[7, 6, 5, 4, 3, 2, 1], [0, 1, 2, 3, 4, 5, 6]],
                        {nostroke: false, axis: "0 0 1 1", symbol: "x", smooth: true});
    lines.hoverColumn(function () {
        this.tags = r.set();
        for (var i = 0, ii = this.y.length; i < ii; i++) {
            this.tags.push(r.g.popup(this.x, this.y[i], this.values[i], 150, 10)
                .insertBefore(this)
                .attr([{fill: "#000"}, {fill: this.symbols[i].attr("fill")}]));
        }
    }, function () {
        this.tags && this.tags.remove();
    });
    lines.symbols.attr({r: 3});
    
    // Enables tabs. Refresh tabs.
    $("#tabs").tabify();
});


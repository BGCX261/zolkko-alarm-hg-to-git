
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
    $.fn.tabify = function () {
        return this;
    }
})(jQuery);

$(function () {
    $("#c0").addbox();
    $("#c2").addbox();
    $("#c3").addbox();
    $("#c4").addbox();
});


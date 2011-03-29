
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

	var __createBox = function (container, title, cnt) {
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

	$.fn.addbox = function (title, cnt) {
		return this.each(function (i) {
			var o = $(this);
			return __createBox(o, title, cnt);
		});
	};
})(jQuery);

$(function () {
	// cache
	var __randtxt = "This text is template for random text. ";
	var page = $("#page");
	var lastRow = $("r1");
    var defWidth = 250;

	var randText = function () {
		var count = Math.round(Math.random() * 11);
		if (count == 0) {
			count = 13;
		}
		var str = new Array();
		for (var i = 0; i < count; i++) {
			str.push(__randtxt);
		}
		return str.join(" ");
	};

	var randomize = function (e) {
		var dialogs = $(".dlg");
		var count = dialogs.length;
		var pw = Math.pow(10, Math.ceil(count / 3));
		for (var i = 0; i < count; i++) {
			var j = Math.floor(Math.random() * pw % count);
			if (i != j) {
				var from = $(dialogs.get(i));
				var to = $(dialogs.get(j));
				var tmp = from.parent();
				to.parent().append(from);
				tmp.append(to);
			}
		}
		return false;
	};

	var appendBox = function (e) {
		var dialogs = $(".dlg");
		var mod = dialogs.length % 3;
		var row = Math.ceil(dialogs.length / 3);
		if (mod == 0) {
			lastRow = $("<div></div>").attr("class", "row");
			page.append(lastRow);
			row += 1;
		}
		lastRow.append($("<div></div>")
			.attr("class", "cell")
			.addbox("Row " + row + " cell " + mod, randText()));
		return false;
	};

	var incWidth = function (e) {
		defWidth += 15;
		$(".dlg").width(defWidth);
		$(".cell").css("min-width", defWidth + "px");
		return false;
	};

	var decWidth = function (e) {
		defWidth -= 15;
		$(".dlg").width(defWidth);
		$(".cell").css("min-width", defWidth + "px");
		return false;
	};

    $("#c0").addbox("Показатели датчиков");
    $("#c1").addbox("Параметры", "");
    $("#c2").addbox("Операция", "");

    /*
	$("#c0").addbox("Select action",
			$("<ul></ul>")
				.append($("<li></li>")
					.append($('<a href="#">Add block with random text</a>').click(appendBox)))
				.append($("<li></li>")
						.append($('<a href="#">Resort blocks randomly</a>').click(randomize)))
				.append($("<li></li>")
					.append($('<a href="#">Increas block width</a>').click(incWidth)))
				.append($("<li></li>")
					.append($('<a href="#">Decreas block width</a>').click(decWidth))));
	$("#c1").addbox("Задание", "На JavaScript добавляются блоки перезаданной ширины со случайной длинной текста. Добавление должно происходить всегда в конец списка блоков. <p>Заголовки блоков находятся на одном уровне.</p> <p>Resort block - случайным образом задаёт порядок следования уже существующих блоков.</p> <p>Увеличение/уменьшение блоков должно приводить при необходимости к переносу блоков на новый ряд.</p> ");
	$("#c2").addbox("Дополнительно", "При наличи времени добавить возможность ручной сортировки блоков, перетаскивая их за заголовки. <p>Использование сторонних библиотек для этой части задания остаётся на ваше усмотрение.</p> ");

	$("[rel=auto]").each(function (i) {
		$(this).addbox("Auto box #" + i, "Some text Some Text Some Text Some Text Some Text");
	});
    */
});


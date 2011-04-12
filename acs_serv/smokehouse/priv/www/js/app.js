
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
    var __tabify = function (self) {
        var selectedId = null;
        
        $(tabs).find("ul.tabs li").each(function (index, value) {
            var idx = index;
            var obj = $(value);
            var id = obj.attr("id");
            
            if (obj.hasClass("selected")) {
                selectedId = id;
                $("#" + id + "_page").css("display", "block");
               
                self.trigger("tab.changed");
            }
            
            obj.click(function () {
                if (id != selectedId) {
                    $("#" + selectedId).removeClass("selected");
                    $("#" + selectedId + "_page").css("display", "none");
                }
                
                $("#" + id).addClass("selected");
                $("#" + id + "_page").css("display", "block");
                selectedId = id;
               
                self.trigger("tab.changed");
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

(function ($) {
    var _log = [];
    
    $.info = function (msg) {
        var d = new Date();
        _log.append({"type": "info", "message": msg, "time": d});
        if (console && console.info) {
            console.info("[" + d + "] " + msg);
        }
        $.jnotify(msg);
    }
    
    $.error = function (msg) {
        var d = new Date();
        _log.append({"type": "error", "message": msg, "time": d});
        if (console && console.log) {
            cnosole.info("[" + d + "] " + msg);
        }
        $.jnotify(msg);
    }
    
    $.replayLog = function (count) {
        var i = _log.length - ( count || 10 );
        if (i < 0) {
            i = 0;
        }
        for (; i < _log.length; i++) {
            if (_log[i].type == "error") {
                $.jnotify(_log[i].message, "error");
            } else {
                $.jnotify(_log[i].message);
            }
        }
    }
})(jQuery);

$(function () {
    $(".wnd").addbox();
   
    // TODO: jQuery localization
    $("#node_test").click(function (e) {
        var oUrl = $("#node_url");
        var nodeUrl = oUrl.val().trim();
        if (nodeUrl.length == 0) {
            $.error("Необходимо заполнить поле адрес.");
            oUrl.focus();
            return;
        }
        
        var self = $(this);
        self.attr("disabled", "disabled");
        $.post("node_test", {"address": nodeUrl},
            function (data) {
                if (data.result) {
                    // TODO: use jQuery tamplate and localization features
                    $("#node_stat").empty().append("<img src=\"img/accept.png\" alt=\"accept\" />");
                    $.info("Узел " + nodeUrl + " доступен.");
                } else {    
                    $("#node_stat").empty().append("<img src=\"img/error.png\" alt=\"error\" />");
                    $.error("Узел " + nodeUrl + " не доступен.");
                }
            }, "text/json")
            .error(function (error) {
                if (console && console.log) {
                    console.log(error);
                }
                $.error(error.status + " Ошибка тестирования доступности узла.");
                $("#node_stat").empty().append("<img src=\"img/error.png\" alt=\"error\" />");
            })
            .complete(function () {                
                self.attr("disabled", "");
            });
    });
    
    $("#node_add").click(function (e) {
        var oName = $("#node_name");
        var oUrl = $("#node_url");
        
        var nodeName = oName.val().trim();
        var nodeUrl = oUrl.val().trim();
        
        if (nodeName.length == 0) {
            $.error("Заполните поле \"имя\" добавляемого узла.");
            $("#node_name").focus();
            return;
        }
        
        if (nodeUrl.length == 0) {
            $.jnotify("Заполните поле \"адрес\" добавляемого узла.", "error");
            $("#node_url").focus();
            return;
        }
        
        var self = $(this);
        self.attr("disabled", "disabled");
        
        $.post("node_add", {"address": nodeUrl, "name": nodeName},
            function (data) {
                if (data.result) {
                    var statNode = $("<span></span>");
                    var tr = $("#node_tbl tr:last").after($("<tr></tr>").append(statNode));
                    tr.append($("<td></td>").text(nodeName));
                    tr.append($("<td></td>").text(nodeUrl));
                    tr.append($("<td></td>")
                        .append($("<input type=\"button\" value=\"Тест доступности\" />").click(function () {
                            $.jnotify("Тест доступности узла " + nodeUrl  + ".");
                            statNode.empty().html("<img src=\"img/error.png\" alt=\"error\" />");
                        }))
                        .append($("<input type=\"button\" value=\"Вкл.\" />").click(function () {
                            var btn = $(this);
                            $.jnotify("Включение новго узла в список слежения");
                        })));
                    oName.val("");
                    oUrl.val("");
                } else {
                    $.jnotify("Узел \"" + nodeName + "\" не добавлен в список слежения.<br />" + data.rason, "error");
                }
            }, "json")
            .error(function (error) {
                if (console && console.log) {
                    console.log(error);
                }
                $.jnotify(error.status + " Ошибка добавления узла \"" + nodeName + "\".", "error");
            })
            .complete(function () {                
                self.attr("disabled", "");
            });
    });
    
    function SmokeHouseView(aName) {
        this.name = aName;
    }
    
    SmokeHouseView.prototype = {
        init: function () {
            //
        },
        
        activate: function () {
            // TODO: Activate smoke house view
        }
    };
	
	// Plotter
	function SensorView (container) {
		this.totalPoints = 30;
        
        this.element = container;
		
        // TODO:
		this.dataset = dataset = [
			{label: "Сухой &deg;C", data: []},
			{label: "Влажный &deg;C", data: []},
			{label: "Внешний &deg;C", data: []},
			{label: "Влажность %", data: []}
		];
		for (var i = 0; i < this.dataset.length; i++) {
			for (var j = 0; j < this.totalPoints; j++) {
					this.dataset[i].data.push([j, 22]);
			}
		}
       
        var w = this.element.parent().width() || 100;
        this.element.css("width", w + "px");
		
		this.plot = $.plot(container, this.dataset, {
			series: {shadowSize: 0},
			yaxis: {min: -10, max: 125},
			xaxis: {show: false, mode: "time"}
		});
	}
	
	SensorView.prototype = {
        resize: function () {
            this.element.css("width", "100%");
            this.plot.resize();
            this.plot.setupGrid();
            this.plot.draw();
        },
        
		update: function (url, onBeforeUpdate, onAfterUpdate) {
			if ($.isFunction(onBeforeUpdate)) {
				onBeforeUpdate();
			}
			
			function normalizeValue(value) {
				var result = parseFloat(value, 10);
				if (result < 0) {
					result = 0;
				} else if (result > 120) {
					result = 100;
				}
				return result;
			}
			
			var self = this;
			$.getJSON(url, function (data, textStatus, jqXHD) {
				for (var i = 0; i < self.dataset.length; i++) {
					for (var j = 1; j < self.totalPoints; j++) {
						self.dataset[i].data[j - 1][1] = self.dataset[i].data[j][1];
					}
				}
				var last = self.totalPoints - 1;
				for (var i = 0; i < data.length; i++) {
					self.dataset[i].data[last][1] = normalizeValue(data[i].value);
				}
				self.plot.setData(self.dataset);
				self.plot.draw();
			})
			.error(function (err) {
				if (console && console.log) {
					console.log(err);
				}
			})
			.complete(function () {
				if ($.isFunction(onAfterUpdate)) {
					onAfterUpdate();
				}
			});
		}
	};
	
	var sensor = new SensorView($("#plot"));
	sensor.update("sensor");
    
    $("#tabs").bind("tab.changed", function () {
        sensor.resize();
    }).tabify();
    
    $(window).bind("resize", function () { sensor.resize(); });
    
    $("#next").click(function () {
        $(this).attr("disabled", "disabled");
        $("#terminate").attr("disabled", "disabled");
        $.get("next_operation", function (data, statusText, jqXHD) {
        })
        .error(function () {
            alert("Operation failed");
        })
        .complete(function () {
            $("#next").attr("disabled", "");
            $("#terminate").attr("disabled", "");
        });
    });
    
    $("#terminate").click(function () {
        $("#next").attr("disabled", "disabled");
        $(this).attr("disabled", "disabled");
        $.get("terminate_operation", function (data, statusText, jqXHD) {
            //
        })
        .error(function () {
            //
        })
        .complete(function () {
            $("#next").attr("disabled", "");
            $("#terminate").attr("disabled", "");
        });
    });
    
    $("#update_time").click(function () {
        $(this).attr("disabled", "disabled");
        $.post("update_time", {"time": $("#time").val().trim()}, function (data, textStatus, jqXHD) {
            //
        }, "text/json")
        .error(function (error) {
            alert(error.responseText);
        })
        .complete(function () {
            $("#update_time").attr("disabled", "");
        });
    });
	
	$("#node_refresh").click(function () {
		var self = $(this);
		sensor.update("sensor",
			function () {
				self.attr("disabled", "disabled")
			},
			function () {
				self.attr("disabled", "")
			});
	});
	
	function load_psy_table_version() {
		$.getJSON("psy_table_version", function (psyTable, textStatus, jqXHD) {
			$("#psy_table_version").text(psyTable.version);
		})
		.error(function () {
			$("#psy_table_version").text("Ошибка");
		});
	}
    
    // Get status
    $.getJSON("status", function (list, textStatus, jqXHR) {
        var ostat = $("#stat").empty();
        if (ostat) {
            if (list && list.length) {
                var currentTemperature = 22;
				var currentTask = -1;
                for (var i = 0; i < list.length; i++) {
                    var item = list[i];
                    var text = item.name + ". Длительность " + item.duration + " мин. ";
                    
                    if (item.temperature.dynamic) {
                        var newTemperature = parseInt(item.temperature.value, 10);
                        if (newTemperature > currentTemperature) {
                            text += "Повысить температуру до " + newTemperature + "&deg;C. ";
                        } else if (newTemperature < currentTemperature) {
                            text += "Понизить температуру до " + newTemperature + "&deg;C. ";
                        } else {
                            text += "Оставить температуру на преджнем уровне. ";
                        }
                        currentTemperature = newTemperature;
                    } else {
                        currentTemperature = parseInt(item.temperature.value, 10);
                        text += "Температура " + item.temperature.value + "&deg;C. ";
                    }
                    
                    if (item.smog) {
                        text += "Включить подачу дыма. ";
                    }
                    
                    var listItem = $("<li></li>");
                    if (item.finished) {
                        listItem.addClass("finished");
                    } else if (currentTask == -1 && item.start_stamp) {
                        text += "<img src=\"img/running_green_transparent.gif\" alt=\"in progress\" />";
						currentTask = i;
                    }
                    
                    ostat.append(listItem.html(text));
                }
				
				if (currentTask == list.length) {
					$("#next").attr("disabled", "");
				} else {
					$("#next").attr("disabled", "disabled");
				}
				
				$("#next").attr("disabled", (currentTask != -1 && currentTask < list.length ? "" : "disabled"));
				$("#terminate").attr("disabled", (list.length > 0 ? "" : "disabled"));
            } else {
                $("#next, #terminate").attr("disabled", "disabled");
            }
        }
		
		load_psy_table_version();
    });
});



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

$(function () {
    $(".cell").addbox();
    
    $("#sensor_test").click(function (e) {
        var self = $(this);
        self.attr("disabled", "disabled");
        $.post("sensor_test", {"address": $("#sensor_url").val()},
            function (data) {
                if (data.result) {
                    $("#sensor_stat").empty().append("<img src=\"img/accept.png\" alt=\"accept\" />");
                    $.jnotify("Host " + $("#sensor_url").val() + " is accessible.");
                } else {    
                    $("#sensor_stat").empty().append("<img src=\"img/error.png\" alt=\"error\" />");
                    $.jnotify("Host " + $("#sensor_url").val() + " is unaccessible.");
                }
            }, "text/json")
            .error(function (error) {
                if (console && console.log) {
                    console.log(error);
                }
                $.jnotify("Faild to test host accessebility", "error");
                $("#sensor_stat").empty().append("<img src=\"img/error.png\" alt=\"error\" />");
            })
            .complete(function () {                
                self.attr("disabled", "");
            });
    });
    
    $("#sensor_add").click(function (e) {
        var self = $(this);
        self.attr("disabled", "disabled");
        var postData = {"address": $("#sensor_url").val(), "name": $("#sensor_name").val()};
        $.post("sensor_add", postData,
            function (data) {
                if (data.result) {
                    var tr = $("#sensor_tbl tr:last").after("<tr></tr>");
                    tr.append($("<td><span id=\"sensor_1_stat\"></span></td>").text($("#sensor_name").val()));
                    tr.append($("<td></td>").text($("#sensor_url").val()));
                    tr.append($("<td></td>")
                        .append($("<input type=\"button\" value=\"Тест\" />").click(function () {
                            // TODO: test
                        }))
                        .append($("<input type=\"button\" value=\"Вкл.\" />").click(function () {
                            // TODO: open box
                        })));
                    $("#sensor_name").val("");
                    $("#sensor_url").val("");
                } else {
                    // TODO: false
                }
            }, "json")
            .error(function (error) {
                if (console && console.log) {
                    console.log(error);
                }
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
	
	$("#sensor_refresh").click(function () {
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


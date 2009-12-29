/*
 * jQuery UI Autocomplete 1.8pre
 *
 * Copyright (c) 2009 AUTHORS.txt (http://jqueryui.com/about)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 *
 * http://docs.jquery.com/UI/Autocomplete
 *
 * Depends:
 *	jquery.ui.core.js
 *	jquery.ui.widget.js
 */
(function($) {

$.widget("ui.autocomplete", {
	options: {
		minLength: 1,
		delay: 300
	},
	_init: function() {
		var self = this;
		this.cache = {};
		this.element
			.addClass("ui-autocomplete")
			.attr("autocomplete", "off")
			// TODO verify these actually work as intended
			.attr({
				role: "textbox",
				"aria-autocomplete": "list",
				"aria-haspopup": "true"
			})
			.bind("keydown.autocomplete", function(event) {
				var keyCode = $.ui.keyCode;
				switch(event.keyCode) {
				case keyCode.PAGE_UP:
					self.move("previousPage");
					break;
				case keyCode.PAGE_DOWN:
					self.move("nextPage");
					break;
				case keyCode.UP:
					self.move("previous");
					event.preventDefault();
					break;
				case keyCode.DOWN:
					self.move("next");
					event.preventDefault();
					break;
				case keyCode.ENTER:
				case keyCode.TAB:
					if (!self.menu || !self.menu.active) {
						return;
					}
					event.preventDefault();
					self.select();
					break;
				case keyCode.ESCAPE:
					self.element.val(self.term);
					self.close();
					break;
				case 16:
				case 17:
				case 18:
					// ignore metakeys (shift, ctrl, alt)
					break;
				default:
					// keypress is triggered before the input value is changed
					clearTimeout(self.searching);
					self.searching = setTimeout(function() {
						self.search();
					}, self.options.delay);
					break;
				}
			})
			.bind("focus.autocomplete", function() {
				self.previous = self.element.val();
			})
			.bind("blur.autocomplete", function() {
				clearTimeout(self.searching);
				// clicks on the menu (or a button to trigger a search) will cause a blur event
				// TODO try to implement this without a timeout, see clearTimeout in search()
				self.closing = setTimeout(function() {
					// TODO pass {data: item} when a valid value is selected, even when the suggestionlist wasn't used
					self.close();
				}, 150);
			});
		this.initSource();
	},

	destroy: function() {
		this.element
			.unbind(".autocomplete")
			.removeClass("ui-autocomplete")
			.removeAttr("autocomplete")
			.removeAttr("role")
			.removeAttr("aria-autocomplete")
			.removeAttr("aria-haspopup");
		$.Widget.prototype.destroy.call(this);
	},

	// TODO call when source-option is updated
	initSource: function() {
		if ($.isArray(this.options.source)) {
			var array = this.options.source;
			this.source = function(request, response) {
				// escape regex characters
				var matcher = new RegExp($.ui.autocomplete.escapeRegex(request.term), "i");
				return $.grep(array, function(value) {
    				return matcher.test(value);
				});
			};
		} else if (typeof this.options.source == "string") {
			var url = this.options.source;
			this.source = function(request, response) {
				$.getJSON(url, request, response);
			};
		} else {
			this.source = this.options.source;
		}
	},

	search: function(value) {
		var self = this;
		clearTimeout(self.closing);
		value = value !== undefined ? value : this.element.val();
		if (value.length >= this.options.minLength) {
			if (this._trigger("search") === false) {
				return;
			}
			self.element.addClass("ui-autocomplete-loading");
			// always save the actual value, not the one passed as an argument
			self.term = this.element.val();
			function response(content) {
				if (content.length) {
					content = self.normalize(content);
					self._trigger("open");
					self.suggest(content);
				} else {
					self.close();
				}
				self.element.removeClass("ui-autocomplete-loading");
			}
			// source can call response or return content directly
			var result = this.source({ term: value }, response);
			if (result) {
				response(result);
			}
		} else {
			self.close();
		}
	},

	close: function(selected) {
		clearTimeout(this.closing);
		if (this.menu) {
			this._trigger("close");
			this.menu.element.remove();
			this.menu = null;
		}
		if (this.element.val().length >= this.options.minLength && this.previous != this.element.val()) {
			this._trigger("change", null, selected);
		}
	},

	normalize: function(items) {
		// TODO consider optimization: if first item has the right format, assume all others have it as well
		return $.map(items, function(item) {
			if (typeof item == "string") {
				return {
					label: item,
					result: item
				};
			}
			return $.extend({
				label: item.label || item.result,
				result: item.result || item.label
			}, item);
		});
	},

	suggest: function(items) {
		(this.menu && this.menu.element.remove());
		var self = this,
			ul = $("<ul></ul>");
		$.each(items, function(index, item) {
			$("<li></li>")
				.data("item.autocomplete", item)
				.append("<a>" + item.label + "</a>")
				.appendTo(ul); 
		});
		ul
			.addClass("ui-autocomplete-menu")
			.appendTo(document.body)
			.menu({ 
				focus: function(event, ui) {
					self._trigger("focus", null, { item: ui.item.data("item.autocomplete") });
					// use result to match what will end up in the input
					self.element.val(ui.item.data("item.autocomplete").result);
				},
				selected: function(event, ui) {
					var data = ui.item.data("item.autocomplete");
					self.element.val( data.result );
					self.close({ item: data });
					// prevent the blur handler from triggering another change event
					self.previous = data.result;
					// TODO only trigger when focus was lost?
					self.element.focus();
				} 
			})
			.removeClass("ui-corner-all").addClass("ui-corner-bottom")
			.position({ 
				my: "left top", 
				at: "left bottom", 
				of: this.element 
			});
		this.menu = ul.data("menu");
		if (ul.width() <= this.element.width()) {
			ul.width(this.element.width());
		}
	},

	move: function(direction) {
		if (!this.menu) {
			this.search();
			return;
		}
		if (this.menu.first() && /^previous/.test(direction) || this.menu.last() && /^next/.test(direction)) {
			this.element.val(this.term);
			this.menu.deactivate();
			return;
		}
		this.menu[direction]();
	},

	select: function() {
		this.menu.select();
	},

	widget: function() {
		// return empty jQuery object when menu isn't initialized yet
		return this.menu && this.menu.element || $([]);
	}
});

$.extend($.ui.autocomplete, {
	escapeRegex: function(value) {
		return value.replace(/([\^\$\(\)\[\]\{\}\*\.\+\?\|\\])/gi, "\\$1");
	}
});

})(jQuery);

/*
 * jQuery UI Menu 1.8pre
 * 
 * This widget isn't yet finished and the API is subject to change. We plan to finish
 * it for the next release. You're welcome to give it a try anyway and give us feedback,
 * as long as you're okay with migrating your code later on. We can help with that, too.
 *
 * Copyright (c) 2009 AUTHORS.txt (http://jqueryui.com/about)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 *
 * http://docs.jquery.com/UI/Menu
 *
 * Depends:
 *	jquery.ui.core.js
 */
(function($) {

$.widget("ui.menu", {
	_init: function() {
		var self = this;
		this.element
			.addClass("ui-menu ui-widget ui-widget-content ui-corner-all")
			.attr({
				role: "menu",
				"aria-activedescendant": "ui-active-menuitem"
			})
			.click(function(e) {
				// temporary
				e.preventDefault();
				self.select();
			});
		var items = this.element.children("li")
			.addClass("ui-menu-item")
			.attr("role", "menuitem");
		
		items.children("a")
			.addClass("ui-corner-all")
			.attr("tabindex", -1)
			// mouseenter doesn't work with event delegation
			.mouseenter(function() {
				self.activate($(this).parent());
			});
	},

	activate: function(item) {
		this.deactivate();
		this.active = item.eq(0)
			.children("a")
				.addClass("ui-state-hover")
				.attr("id", "ui-active-menuitem")
			.end();
		this._trigger("focus", null, { item: item });
		if (this.hasScroll()) {
			var offset = item.offset().top - this.element.offset().top,
				scroll = this.element.attr("scrollTop"),
				elementHeight = this.element.height();
			if (offset < 0) {
				this.element.attr("scrollTop", scroll + offset);
			} else if (offset > elementHeight) {
				this.element.attr("scrollTop", scroll + offset - elementHeight + item.height());
			}
		}
	},

	deactivate: function() {
		if (!this.active) { return; }

		this.active.children("a")
			.removeClass("ui-state-hover")
			.removeAttr("id");
		this.active = null;
	},

	next: function() {
		this.move("next", "li:first");
	},

	previous: function() {
		this.move("prev", "li:last");
	},

	first: function() {
		return this.active && !this.active.prev().length;
	},

	last: function() {
		return this.active && !this.active.next().length;
	},

	move: function(direction, edge) {
		if (!this.active) {
			this.activate(this.element.children(edge));
			return;
		}
		var next = this.active[direction]();
		if (next.length) {
			this.activate(next);
		} else {
			this.activate(this.element.children(edge));
		}
	},

	// TODO merge with previousPage
	nextPage: function() {
		if (this.hasScroll()) {
			// TODO merge with no-scroll-else
			if (!this.active || this.last()) {
				this.activate(this.element.children(":first"));
				return;
			}
			var base = this.active.offset().top,
				height = this.element.height(),
				result = this.element.children("li").filter(function() {
					var close = $(this).offset().top - base - height + $(this).height();
					// TODO improve approximation
					return close < 10 && close > -10;
				});

			// TODO try to catch this earlier when scrollTop indicates the last page anyway
			if (!result.length) {
				result = this.element.children(":last");
			}
			this.activate(result);
		} else {
			this.activate(this.element.children(!this.active || this.last() ? ":first" : ":last"));
		}
	},

	// TODO merge with nextPage
	previousPage: function() {
		if (this.hasScroll()) {
			// TODO merge with no-scroll-else
			if (!this.active || this.first()) {
				this.activate(this.element.children(":last"));
				return;
			}

			var base = this.active.offset().top,
				height = this.element.height();
				result = this.element.children("li").filter(function() {
					var close = $(this).offset().top - base + height - $(this).height();
					// TODO improve approximation
					return close < 10 && close > -10;
				});

			// TODO try to catch this earlier when scrollTop indicates the last page anyway
			if (!result.length) {
				result = this.element.children(":first");
			}
			this.activate(result);
		} else {
			this.activate(this.element.children(!this.active || this.first() ? ":last" : ":first"));
		}
	},

	hasScroll: function() {
		return this.element.height() < this.element.attr("scrollHeight");
	},

	select: function() {
		this._trigger("selected", null, { item: this.active });
	}
});

})(jQuery);
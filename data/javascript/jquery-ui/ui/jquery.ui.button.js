/*
 * jQuery UI Button 1.8pre
 *
 * Copyright (c) 2009 AUTHORS.txt (http://jqueryui.com/about)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 *
 * http://docs.jquery.com/UI/Button
 *
 * Depends:
 *	jquery.ui.core.js
 *	jquery.ui.widget.js
 */
(function($) {

/*
Plan:
- button plugin
  - button - full support
  - input - only text support
  - radio - full support via label
  - checkbox - full support via label
- buttonSet plugin
:button,:submit,:reset,:checkbox,:radio
  - groups buttons together
  - auto-instantiate children as buttons
  
  -> start by rewriting toggleButton and radioButton to use the label as the button element, instead of creating button elements
*/

var lastActive,
	baseClasses = "ui-button ui-widget ui-state-default ui-corner-all",
	otherClasses = "ui-state-hover ui-state-active " +
		"ui-button-icons-only ui-button-icon-only ui-button-text-icons ui-button-text-icon";

$.widget("ui.button", {
	options: {
		text: true,
		label: null,
		icons: {
			primary: null,
			secondary: null
		}
	},
	_init: function() {
		var options = this.options;

		if (options.label === null) {
			options.label = this.element.html();
		}

		this.element
			.addClass(baseClasses)
			.bind("mouseenter.button", function() {
				if (options.disabled) { return; }
				$(this).addClass("ui-state-hover");
				if (this == lastActive) {
					$(this).addClass("ui-state-active");
				}
			})
			.bind("mouseleave.button", function() {
				if (options.disabled) { return; }
				$(this).removeClass("ui-state-hover ui-state-active");
			})
			.bind("mousedown.button", function() {
				if (options.disabled) { return; }
				$(this).addClass("ui-state-active");
				lastActive = this;
				$(document).one('mouseup', function() {
					lastActive = null;
				});
			})
			.bind("mouseup.button", function() {
				if (options.disabled) { return; }
				$(this).removeClass("ui-state-active");
			});

		this._resetButton();
	},

	destroy: function() {
		this.element.html(this.element.find(".ui-button-text").html());
		this.element
			.removeClass(baseClasses + " " + otherClasses)
			.unbind(".button");
		$.Widget.prototype.destroy.call(this);
	},

	_setOption: function(key, value) {
		$.Widget.prototype._setOption.apply(this, arguments);
		this._resetButton();
	},

	_resetButton: function() {
		$("<span></span>")
			.addClass("ui-button-text")
			.html(this.options.label)
			.appendTo(this.element.empty());

		var icons = this.options.icons,
			multipleIcons = icons.primary && icons.secondary;
		if (icons.primary || icons.secondary) {
			this.element.addClass("ui-button-text-icon" +
				(multipleIcons ? "s" : ""));
			if (icons.primary) {
				this.element.prepend("<span class='ui-button-icon-primary ui-icon " + icons.primary + "'></span>");
			}
			if (icons.secondary) {
				this.element.append("<span class='ui-button-icon-secondary ui-icon " + icons.secondary + "'></span>");
			}
			if (!this.options.text) {
				this.element
					.addClass(multipleIcons ? "ui-button-icons-only" : "ui-button-icon-only")
					.removeClass("ui-button-text-icons ui-button-text-icon");
				if (!this.element.attr("title")) {
					this.element.attr("title", this.element.find(".ui-button-text").text());
				}
			}
		} else {
			this.element.addClass("ui-button-text-only");
		}
	}
});

// TODO merge with button-widget
$.widget("ui.toggleButton", {
	_init: function() {
		this.element.hide();
		var self = this,
			label = (this.label = $("[for='" + this.element.attr("id") + "']"));
		label.button()
			.unbind("mousedown.button mouseup.button mouseleave.button")
			.bind("click.button", function() {
				if (self.options.disabled) { return; }
				$(this).toggleClass("ui-state-active");
				self.element.attr("checked", function() {
					return !this.checked;
				})
				.click();
			})
			.bind("mouseleave.button", function() {
				if (self.options.disabled) { return; }
				$(this).removeClass("ui-state-hover");
			});
			
		if (this.element.attr("checked")) {
			label.addClass("ui-state-active");
		}
	},

	destroy: function() {
		this.element.show();
		this.label.button("destroy");
		$.Widget.prototype.destroy.call(this);
	},
	
	widget: function() {
		return this.button;
	}
});

// TODO merge with button-widget
$.widget("ui.radioButton", {
	_init: function() {
		var self = this,
			radios = (this.radios = this.element.find(":radio"));
		self.labels = $([]);
		self.element.addClass("ui-button-set");
		radios.hide().each(function(index) {
			var radio = $(this),
				label = $("[for='" + this.id + "']");
			var button = label
				.button()
				.unbind("mousedown.button mouseup.button mouseleave.button")
				.bind("click.button", function() {
					if (self.options.disabled) { return; }
					self.labels.removeClass("ui-state-active");
					$(this).addClass("ui-state-active");
					radio.attr("checked", true).click();
				})
				.bind("mouseleave.button", function() {
					if (self.options.disabled) { return; }
					$(this).removeClass("ui-state-hover");
				});

			if (this.checked) {
				button.addClass("ui-state-active");
			}
			
			self.labels = self.labels.add(label);
		});
	},

	destroy: function() {
		this.radios.show();
		this.labels.button("destroy");
		$.Widget.prototype.destroy.call(this);
	},
	
	widget: function() {
		// TODO technically correct, but inconsistent
		return this.radios.next();
	}
});

$.widget("ui.buttons", {
	_init: function() {
		var buttons = (this.buttons = this.element.find("button, :submit, :reset, a").button());
		if (!buttons.length) {
			this.toggle = this.element.find(":checkbox").toggleButton();
			buttons = this.toggle.next();
		}
		if (!buttons.length && this.element.is(":has(:radio)")) {
			this.radio = this.element.radioButton();
			buttons = this.radio.find(".ui-button");
		}
		if (buttons.length) {
			this.element.addClass("ui-button-set");
			buttons.removeClass("ui-corner-all");
			buttons.filter(":first").addClass("ui-corner-left");
			buttons.filter(":last").addClass("ui-corner-right");
		} else {
			this.buttons = this.element.filter("button").button();
		}
	},
	
	destroy: function() {
		if (this.toggle) {
			this.toggle.toggleButton("destroy");
		}
		if (this.radio) {
			this.radio.radioButton("destroy");
		}
		this.buttons.button("destroy").removeClass("ui-corner-left ui-corner-right");
		$.Widget.prototype.destroy.call(this);
	},
	
	widget: function() {
		// TODO technically correct, but inconsistent
		return this.buttons;
	}
});


})(jQuery);

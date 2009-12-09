/*
 * autocomplete_options.js
 */
(function($) {

module("autocomplete: options");


/* disabled until autocomplete actually has built-in support for caching 
// returns at most 4 items
function source(request) {
	ok(true, "handling a request");
	switch(request.term) {
	case "cha":
		return ["Common Pochard", "Common Chiffchaff", "Common Chaffinch", "Iberian Chiffchaff"]
	case "chaf":
	case "chaff":
		return ["Common Chiffchaff", "Common Chaffinch", "Iberian Chiffchaff"]
	case "chaffi":
		return ["Common Chaffinch"]
	case "schi":
		return ["schifpre"]
	}
}

function search(input) {
	var autocomplete = input.data("autocomplete");
	autocomplete.search("cha");
	autocomplete.close();
	autocomplete.search("chaf");
	autocomplete.close();
	autocomplete.search("chaff");
	autocomplete.close();
	autocomplete.search("chaffi");
	autocomplete.close();
	autocomplete.search("schi");
}
	
test("cache: default", function() {
	expect(2);
	search($("#autocomplete").autocomplete({
		source: source
	}));
});

test("cache: {limit:4}", function() {
	expect(3);
	search($("#autocomplete").autocomplete({
		cache: {
			limit: 4
		},
		source: source
	}));
});

test("cache: false", function() {
	expect(5);
	search($("#autocomplete").autocomplete({
		cache: false,
		source: source
	}));
});
*/

test("delay", function() {
});

test("minLength", function() {
});

test("source, url string", function() {
});

test("source, local string array", function() {
});

test("source, local object array", function() {
});

test("source, remote json string array", function() {
});

test("source, remote json object array", function() {
});

})(jQuery);

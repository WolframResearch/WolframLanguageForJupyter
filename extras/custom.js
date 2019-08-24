/* (adapted) from https://stackoverflow.com/a/19961519 by Erik Aigner */
HTMLTextAreaElement.prototype.insertAtCaret = function (text) {
	text = text || '';
	if(document.selection) {
		// IE
		this.focus();
		var sel = document.selection.createRange();
		sel.text = text;
	}
	else if(this.selectionStart || this.selectionStart === 0) {
		// Others
		var startPos = this.selectionStart;
		var endPos = this.selectionEnd;
		this.value = this.value.substring(0, startPos) + text + this.value.substring(endPos, this.value.length);
		this.selectionStart = startPos + text.length;
		this.selectionEnd = startPos + text.length;
	}
	else {
		this.value += text;
	}
};

/* (adapted) from https://stackoverflow.com/a/51114347 by bambam */
function redirectEsc(event) {
	if(event.which == 27)
	{
		event.target.insertAtCaret(
				/* the vertical ellipsis character */
				String.fromCharCode(8942)
			);
		event.stopImmediatePropagation();
	}
}

/* (adapted) from https://stackoverflow.com/a/51114347 by bambam */
var observer = new MutationObserver(function(mutations) {

	Array.from(
			document.querySelectorAll('.input_area')
		).forEach(
				textarea =>
				{
					textarea.removeEventListener('keydown', redirectEsc);
					textarea.addEventListener('keydown', redirectEsc);
				}
		);

});

observer.observe(document, {childList:true, subtree:true});
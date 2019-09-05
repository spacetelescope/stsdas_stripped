define	IS_WHITE	($1 <= ' ')
define	SEP_CHAR	'\t'

# LST_GET -- Get the I-th word from a list

int procedure lst_get (list, item, word, maxch)

char	list[ARB]	# i: list of words
int	item		# i: number of word to extract (zero based)
char	word[ARB]	# o: word extracted from list
int	maxch		# i: maximum length of word
#--
int	iw, ic, jc

begin
	iw = 0
	for (ic = 1; iw < item && list[ic] != EOS; ic = ic + 1) {
	     if (list[ic] == SEP_CHAR)
		iw = iw + 1
	}

	if (iw < item)
	    return (0)

	jc = 1
	while (list[ic] != EOS && list[ic] != SEP_CHAR) {
	    if (jc > maxch)
		break

	    word[jc] = list[ic]
	    ic = ic + 1
	    jc = jc + 1
	}

	word[jc] = EOS
	return (jc - 1)

end

# LST_PARSE -- Convert a list of words to canonical form

procedure lst_parse (list)

char	list[ARB]	# u: list of words
#--
char	blank, quote
int	ic, jc

begin
	jc = 1
	blank = EOS
	quote = EOS

	for (ic = 1; list[ic] != EOS; ic = ic + 1) {
	    if (IS_WHITE(list[ic])) {
		if (quote == EOS) {
		    if (blank == EOS) {
			blank = ' '
			list[jc] = SEP_CHAR
			jc = jc + 1
		    }

		} else {
		    list[jc] = ' '
		    jc = jc + 1
		}

	    } else {
		blank = EOS
		if (list[ic] == '"' || list[ic] == '\'') {
		    if (quote == EOS) {
			quote = list[ic]
		    } else if (quote == list[ic]) {
			quote = EOS
		    } else {
			list[jc] = list[ic]
			jc = jc + 1
		    }
		} else {
		    if (jc < ic)
			list[jc] = list[ic]
		    jc = jc + 1
		}
	    }
	}

	if (blank == EOS || jc == 1) {
	    list[jc] = EOS
	} else {
	    list[jc-1] = EOS
	}

end

# LST_PUT -- Put a word in the I-th position in the list

procedure lst_put (list, item, word, maxch)

char	list[ARB]	# u: list of words
int	item		# i: position of word (zero-based)
char	word[ARB]	# i: word to be added
int	maxch		# i: maximum length of list
#--
int	iw, ic, jc

begin
	iw = 0
	for (ic = 1; iw < item && list[ic] != EOS; ic = ic + 1) {
	     if (list[ic] == SEP_CHAR)
		iw = iw + 1
	}

	while (ic <= maxch && iw < item) {
	    list[ic] = SEP_CHAR
	    ic = ic + 1
	    iw = iw + 1
	} 

	for (jc = 1; ic <= maxch && word[jc] != EOS; jc = jc + 1) {
	    list[ic] = word[jc]
	    ic = ic + 1
	}

	list[ic] = EOS
end

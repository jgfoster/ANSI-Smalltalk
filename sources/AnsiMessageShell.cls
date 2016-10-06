"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #AnsiMessageShell
	instanceVariableNames: 'definitionPresenter errorPresenter namePresenter parameterListPresenter returnValuePresenter synopsisPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiMessageShell guid: (GUID fromString: '{ABE9B0EB-BCCA-4104-8943-55F3BD3E3F53}')!
AnsiMessageShell comment: ''!
!AnsiMessageShell categoriesForClass!Unclassified! !
!AnsiMessageShell methodsFor!

createComponents

	super createComponents.
	definitionPresenter		:= self add: TextPresenter 	new name: 'definition'.
	errorPresenter			:= self add: TextPresenter 	new name: 'errors'.
	namePresenter			:= self add: TextPresenter		new name: 'name'.
	parameterListPresenter	:= self add: ListPresenter		new name: 'parameterList'.
	returnValuePresenter	:= self add: TextPresenter 	new name: 'returnValue'.
	synopsisPresenter		:= self add: TextPresenter		new name: 'synopsis'.
!

onViewOpened

	super onViewOpened.
	self caption: model section , ' -- ' , model protocol name , '>>' , model selector.
	definitionPresenter		value: 	model definition.
	errorPresenter			value: 	model errors.
	namePresenter 			value: 	model name.
	parameterListPresenter	list: 		model parameters.
	returnValuePresenter	value: 	model returnValue.
	synopsisPresenter		value: 	model synopsis.! !
!AnsiMessageShell categoriesFor: #createComponents!public! !
!AnsiMessageShell categoriesFor: #onViewOpened!public! !

!AnsiMessageShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 14 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 416 98 2 8 1143017796 1025 720 0 482 8 4278190080 0 7 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 882 1 914 8 #bePlain 8 '&Plain' 1 1 0 0 0 882 1 914 8 #toggleBold 8 '&Bold' 1 1 0 0 0 882 1 914 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 882 1 914 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 978 4097 834 0 16 98 3 882 1025 914 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 882 1025 914 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 882 1025 914 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 978 4097 882 1 914 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 720 0 8 1961168066 852486 ##(Smalltalk.NullConverter)  0 0 11 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 64 0 70 1] 983302 ##(Smalltalk.MessageSequence)  202 208 98 6 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 41 530 1185 81 720 1778 8 #contextMenu: 98 1 848 720 1778 8 #text: 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Verdana;}}
\viewkind4\uc1\pard\f0\fs16 
\par }
' 720 1778 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 720 1778 8 #isTextModified: 98 1 32 720 1778 8 #resetCharFormat 98 0 720 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 80 2 0 0 60 0 0 0] 98 0 530 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 2314 2336 8 #fixedParentRight 1 2314 2336 8 #fixedParentTop 41 2314 2336 8 #fixedViewTop 81 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 2464 0 482 8 4278190080 0 7 0 0 0 2464 0 8 4294903361 1634 0 0 3 1714 202 208 98 4 1778 1808 98 2 530 1 1 530 1185 41 2464 1778 2032 98 1 2066 3 1 3 2464 1778 2112 98 1 32 2464 1778 8 #setMarginWidths: 98 1 98 2 7 3 2464 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 20 0 0 0] 98 0 2256 0 27 2274 2320 1 2368 1 2400 1 2432 41 410 736 98 18 0 416 98 2 8 1143017796 1025 2928 0 482 816 0 7 834 0 16 98 10 882 1 914 944 8 '&Font...' 1 1 0 0 0 978 4097 882 1 914 1040 8 '&Plain' 1 1 0 0 0 882 1 914 1104 8 '&Bold' 1 1 0 0 0 882 1 914 1168 8 '&Italic' 1 1 0 0 0 882 1 914 1232 8 '&Underlined' 1 1 0 0 0 978 4097 834 0 16 98 3 882 1025 914 1344 8 '&Left' 1 1 0 0 0 882 1025 914 1408 8 '&Centre' 1 1 0 0 0 882 1025 914 1472 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 978 4097 882 1 914 1568 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 2928 0 8 1961168066 1634 0 0 11 0 1666 8 #[0 0 0 0 0 0 0 0 64 0 70 1] 1714 202 208 98 6 1778 1808 98 2 530 1 161 530 1185 337 2928 1778 1888 98 1 3008 2928 1778 1936 98 1 1970 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Verdana;}}
\viewkind4\uc1\pard\f0\fs16 
\par }
' 2928 1778 2032 98 1 2066 3 1 3 2928 1778 2112 98 1 32 2928 1778 2160 2176 2928 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 80 0 0 0 80 2 0 0 248 0 0 0] 98 0 2256 0 27 2274 2320 1 2368 1 2400 161 2314 2336 8 #fixedParentBottom -241 410 2480 98 16 0 416 98 2 8 1140916352 1025 4048 0 482 8 4278190080 0 7 0 0 0 4048 0 8 4294903361 1634 0 0 3 1714 202 208 98 4 1778 1808 98 2 530 161 121 530 1025 41 4048 1778 2032 98 1 2066 3 1 3 4048 1778 2112 98 1 32 4048 1778 2816 98 1 98 2 7 3 4048 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 60 0 0 0 80 2 0 0 80 0 0 0] 98 0 2256 0 27 2274 2320 161 2368 1 2400 121 2432 41 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 4480 0 0 0 7 0 0 0 4480 0 8 4294903297 1634 0 0 0 1714 202 208 98 2 1778 1808 98 2 530 1 121 530 161 41 4480 1778 1936 98 1 8 'Return Value:' 4480 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 60 0 0 0 80 0 0 0 80 0 0 0] 98 0 2256 0 27 2274 2320 1 2314 2336 8 #fixedViewLeft 161 2400 121 2432 41 410 8 ##(Smalltalk.ListView)  98 30 0 416 98 2 8 1140920397 1025 4848 590662 2 ##(Smalltalk.ListModel)  202 208 2176 0 2314 8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 4848 0 8 4294902841 459270 ##(Smalltalk.Message)  8 #displayString 98 0 0 2314 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 4 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 51 8 #right 5074 8 #section 98 0 5074 8 #<= 5312 0 0 4848 0 1 0 0 5218 8 'Parameter' 161 8 #left 5074 8 #name 5312 8 ##(Smalltalk.SortedCollection)  0 0 4848 0 1 0 0 5218 8 'Type(s)' 231 5392 5074 8 #typesString 5312 5074 5344 5312 0 0 4848 0 1 0 0 5218 8 'Captured' 141 5392 5074 8 #isCapturedString 5312 5074 5344 5312 0 0 4848 0 1 0 0 8 #report 2176 0 131169 0 0 1714 202 208 98 2 1778 1808 98 2 530 1 499 530 601 241 4848 1778 1936 98 1 8 '#' 4848 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 249 0 0 0 44 1 0 0 113 1 0 0] 98 0 2256 0 27 2274 2320 1 4816 601 4016 -239 4016 1 410 736 98 18 0 416 98 2 8 1143017796 1025 5856 0 482 8 4278190080 0 7 834 0 16 98 10 882 1 914 944 8 '&Font...' 1 1 0 0 0 978 4097 882 1 914 1040 8 '&Plain' 1 1 0 0 0 882 1 914 1104 8 '&Bold' 1 1 0 0 0 882 1 914 1168 8 '&Italic' 1 1 0 0 0 882 1 914 1232 8 '&Underlined' 1 1 0 0 0 978 4097 834 0 16 98 3 882 1025 914 1344 8 '&Left' 1 1 0 0 0 882 1025 914 1408 8 '&Centre' 1 1 0 0 0 882 1025 914 1472 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 978 4097 882 1 914 1568 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 5856 0 8 1961168066 1634 0 0 11 0 1666 8 #[0 0 0 0 0 0 0 0 64 0 70 1] 1714 202 208 98 6 1778 1808 98 2 530 601 499 530 585 241 5856 1778 1888 98 1 5952 5856 1778 1936 98 1 1970 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Verdana;}}
\viewkind4\uc1\pard\f0\fs16 
\par }
' 5856 1778 2032 98 1 2066 3 1 3 5856 1778 2112 98 1 32 5856 1778 2160 2176 5856 2194 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 1 0 0 249 0 0 0 80 2 0 0 113 1 0 0] 98 0 2256 0 27 2274 2320 601 2368 1 4016 -239 4016 1 234 256 98 12 720 8 'synopsis' 2464 8 'name' 2928 8 'definition' 4048 8 'returnValue' 4848 8 'parameterList' 5856 8 'errors' 0 0 0 0 0 1 0 0 0 0 1 0 0 1714 202 208 98 3 1778 1808 98 2 530 5359 21 530 1201 801 416 1778 1936 98 1 8 'Smalltalk 1998 ANSI Message' 416 1778 8 #updateMenuBar 2176 416 2194 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 10 0 0 10 0 0 0 207 12 0 0 154 1 0 0] 98 7 2464 720 4480 4048 2928 4848 5856 2256 0 27 )! !
!AnsiMessageShell class categoriesFor: #resource_Default_view!public!resources-views! !


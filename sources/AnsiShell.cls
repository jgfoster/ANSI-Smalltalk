"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #AnsiShell
	instanceVariableNames: 'chapterDescriptionPresenter chapterNumberPresenter chapterTitlePresenter classListPresenter conformsToListPresenter globalDescriptionPresenter globalListPresenter globalNamesPresenter messageListPresenter protocolDescriptionPresenter protocolListPresenter protocolNamePresenter sectionPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
AnsiShell guid: (GUID fromString: '{1E9A27A7-0036-4FF4-A2B3-333FA62CE186}')!
AnsiShell comment: 'AnsiShell installDefault.'!
!AnsiShell categoriesForClass!Unclassified! !
!AnsiShell methodsFor!

changedClass

	| assoc |
	(assoc := classListPresenter selectionOrNil) isNil ifTrue: [^self].
	self select: assoc in: protocolListPresenter.
!

changedGlobal

	| assoc |
	(assoc := globalListPresenter selectionOrNil) isNil ifTrue: [^self].
	self select: assoc in: protocolListPresenter.
!

changedProtocol

	| assoc protocol |
	assoc := protocolListPresenter selection.
	self
		select: assoc in: classListPresenter;
		select: assoc in: globalListPresenter;
		yourself.
	protocol := assoc value.
	chapterDescriptionPresenter	value: 	protocol chapter description.
	chapterNumberPresenter 		value: 	protocol chapter section.
	chapterTitlePresenter 		value: 	protocol chapter name.
	conformsToListPresenter		list:		protocol conformsToAll.
	globalDescriptionPresenter	value: 	protocol globalDescription.
	globalNamesPresenter		value: 	protocol globalNames.
	messageListPresenter		list:		protocol messages asSortedCollection.
	protocolDescriptionPresenter	value: 	protocol description.
	protocolNamePresenter 		value: 	protocol name.
	sectionPresenter 			value: 	protocol section.

	Keyboard default isShiftDown ifTrue: [protocol inspect].
!

choseConformsTo

	self selectProtocol: conformsToListPresenter selection.
!

createComponents

	super createComponents.
	chapterDescriptionPresenter	:= self add: TextPresenter	new name: 'chapterDescription'.
	chapterNumberPresenter		:= self add: TextPresenter	new name: 'chapterNumber'.
	chapterTitlePresenter		:= self add: TextPresenter	new name: 'chapterTitle'.
	classListPresenter 			:= self add: ListPresenter 	new name: 'classList'.
	conformsToListPresenter		:= self add: ListPresenter	new name: 'conformsToList'.
	globalDescriptionPresenter 	:= self add: TextPresenter	new name: 'globalDescription'.
	globalListPresenter 			:= self add: ListPresenter 	new name: 'globalList'.
	globalNamesPresenter		:= self add: TextPresenter	new name: 'globalNames'.
	messageListPresenter		:= self add: ListPresenter	new name: 'messageList'.
	protocolDescriptionPresenter	:= self add: TextPresenter	new name: 'protocolDescription'.
	protocolListPresenter 		:= self add: ListPresenter 	new name: 'protocolList'.
	protocolNamePresenter		:= self add: TextPresenter	new name: 'protocolName'.
	sectionPresenter			:= self add: TextPresenter	new name: 'section'.
!

createSchematicWiring

	super createSchematicWiring.
	classListPresenter 		when: #selectionChanged 	send: #changedClass 	to: self.
	conformsToListPresenter	when: #actionPerformed	send: #choseConformsTo	to: self.
	globalListPresenter 		when: #selectionChanged 	send: #changedGlobal 	to: self.
	protocolListPresenter 	when: #selectionChanged 	send: #changedProtocol 	to: self.
	messageListPresenter	when: #actionPerformed	send: #openMessage		to: self.
!

fileTypes

	^#(
		#('STB Files (*.STB)' '*.STB')
		#('All Files (*.*)' '*.*')
	).
!

findMessage

	| selector messageList protocolList protocol message |
	selector := ChoicePrompter 
		choices: model messages keys asSortedCollection
		caption: 'Select message'.
	selector isNil ifTrue: [^self].
	messageList := model messages at: selector.
	protocolList := messageList collect: [:each | each protocol].
	protocol := ChoicePrompter
		choices: protocolList
		caption: 'Select protocol'.
	protocol isNil ifTrue: [^self].
	self selectProtocol: protocol.
	message := messageList detect: [:each | each protocol = protocol].
	self selectMessage: message.
	self openMessage.
!

onViewOpened
	
	self readModel.
	super onViewOpened.
	classListPresenter 		list: model classes  	associations asSortedCollection.
	globalListPresenter 		list: model globals  	associations asSortedCollection.
	protocolListPresenter 	list: model protocols  	associations asSortedCollection.
	classListPresenter selections: (Array with: (classListPresenter list detect: [:each | each key = 'Object'])).
!

openMessage

	AnsiMessageShell showOn: messageListPresenter selection.
!

readModel

	| path |
	model notNil ifTrue: [^self].
	path := 'M:\Documents\Dolphin\ANSI\ANSI.STB'.
	(File exists: path) ifFalse: [
		path := FileOpenDialog new 
			fileTypes: self fileTypes;
			caption: 'ANSI Standard Data File';
			defaultExtension: 'STB';
			value: 'ANSI.STB';
			showModal.
		path isNil ifTrue: [^self].
	].
	model := AnsiStandard readFromPath: path.
!

select: anObject in: aPresenter

	anObject isNil ifTrue: [
		aPresenter clearSelection.
	] ifFalse: [
		| item |
		item := aPresenter list 
			detect: [:each | each value = anObject value] 
			ifNone: [nil].
		aPresenter selectionOrNil ~= item ifTrue: [
			aPresenter selectionOrNil: item.
		].
	].
!

selectMessage: aMessage

	| assoc rightGroup messageTab |
	assoc := messageListPresenter list detect: [:each | each = aMessage].
	rightGroup := view viewNamed: 'rightGroup'.
	messageTab := rightGroup viewNamed: 'messagesTab'.
	rightGroup ensureSubViewVisible: messageTab.
	messageListPresenter selection: assoc.
!

selectProtocol: aProtocol

	| assoc leftGroup protocolTab |
	assoc := protocolListPresenter list detect: [:each | each value = aProtocol].
	leftGroup := view viewNamed: 'leftGroup'.
	protocolTab := leftGroup viewNamed: 'protocolList'.
	leftGroup ensureSubViewVisible: protocolTab.
	protocolListPresenter selection: assoc.
! !
!AnsiShell categoriesFor: #changedClass!public! !
!AnsiShell categoriesFor: #changedGlobal!public! !
!AnsiShell categoriesFor: #changedProtocol!public! !
!AnsiShell categoriesFor: #choseConformsTo!public! !
!AnsiShell categoriesFor: #createComponents!public! !
!AnsiShell categoriesFor: #createSchematicWiring!public! !
!AnsiShell categoriesFor: #fileTypes!public! !
!AnsiShell categoriesFor: #findMessage!public! !
!AnsiShell categoriesFor: #onViewOpened!public! !
!AnsiShell categoriesFor: #openMessage!public! !
!AnsiShell categoriesFor: #readModel!public! !
!AnsiShell categoriesFor: #select:in:!public! !
!AnsiShell categoriesFor: #selectMessage:!public! !
!AnsiShell categoriesFor: #selectProtocol:!public! !

!AnsiShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1201 801 551 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 86 101 114 100 97 110 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 2 410 8 ##(Smalltalk.CardContainer)  98 16 0 416 98 2 8 1140850688 131073 720 0 482 8 4278190080 0 7 0 0 0 720 655878 ##(Smalltalk.CardLayout)  202 208 98 3 721414 ##(Smalltalk.Association)  8 'Classes' 410 8 ##(Smalltalk.ListBox)  98 17 0 720 98 2 8 1144062209 1025 944 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 8 4278190080 0 7 0 0 0 944 0 8 4294903387 459270 ##(Smalltalk.Message)  8 #key 98 0 1072 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 9 45 530 459 645 944 1330 8 #horizontalExtent: 98 1 1 944 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 233 0 0 0 88 1 0 0] 98 0 530 193 193 0 27 898 8 'Globals' 410 960 98 17 0 720 98 2 8 1144062209 1025 1584 1026 202 208 1072 0 1104 482 1168 0 5 0 0 0 1584 0 8 4294903387 1202 1232 1248 1072 0 1266 202 208 98 2 1330 1360 98 2 530 9 45 530 459 645 1584 1330 1440 98 1 1 1584 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 233 0 0 0 88 1 0 0] 98 0 1536 0 27 898 8 'Protocols' 410 960 98 17 0 720 98 2 8 1144062209 1025 1952 1026 202 208 1072 0 1104 482 1168 0 5 0 0 0 1952 0 8 4294903387 1202 1232 1248 1072 0 1266 202 208 98 2 1330 1360 98 2 530 9 45 530 459 645 1952 1330 1440 98 1 1 1952 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 233 0 0 0 88 1 0 0] 98 0 1536 0 27 944 234 256 98 6 1952 8 'protocolList' 944 8 'classList' 1584 8 'globalList' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 720 98 2 8 1140916736 1 2368 1026 202 208 98 3 928 1568 1936 0 1104 0 0 1 0 0 0 2368 0 8 4294903405 787814 3 ##(Smalltalk.BlockClosure)  0 0 918822 ##(Smalltalk.CompiledMethod)  2 3 8 ##(Smalltalk.ListControlView)  8 #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] 8 #displayString 2528 7 257 0 2514 0 0 2546 2 3 8 ##(Smalltalk.IconicListAbstract)  8 #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] 8 #iconImageIndex 2640 7 257 0 1098 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 8 #noIcons 0 0 0 0 0 1266 202 208 98 3 1330 1360 98 2 530 1 1 530 475 697 2368 1330 8 #selectionByIndex:ifAbsent: 98 2 3 1330 8 #yourself 1072 0 2368 1330 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 2368 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 237 0 0 0 92 1 0 0] 98 0 1536 0 27 1266 202 208 98 1 1330 1360 98 2 530 1 1 530 475 697 720 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 237 0 0 0 92 1 0 0] 98 4 944 1584 1952 2368 1536 0 27 524806 ##(Smalltalk.Fraction)  801 1175 32 234 256 98 4 720 8 'leftGroup' 410 736 98 16 0 416 98 2 8 1140850688 131073 3328 0 482 8 4278190080 0 7 0 0 0 3328 834 202 208 98 5 898 8 'Protocol' 410 8 ##(Smalltalk.ContainerView)  98 15 0 3328 98 2 8 1140850688 131073 3504 0 0 0 5 0 0 0 3504 852230 ##(Smalltalk.FramingLayout)  234 240 98 6 410 8 ##(Smalltalk.TextEdit)  98 16 0 3504 98 2 8 1140916352 1025 3648 0 482 8 4278190080 0 5 0 0 0 3648 0 8 4294903361 852486 ##(Smalltalk.NullConverter)  0 0 3 1266 202 208 98 4 1330 1360 98 2 530 1 1 530 91 41 3648 1330 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 3648 1330 8 #isTextModified: 98 1 32 3648 1330 8 #setMarginWidths: 98 1 98 2 7 3 3648 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 45 0 0 0 20 0 0 0] 98 0 1536 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1098 8 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 1 1098 4208 8 #fixedViewLeft 91 1098 4208 8 #fixedParentTop 1 1098 4208 8 #fixedViewTop 41 410 3664 98 16 0 3504 98 2 8 1140916352 1025 4336 0 482 3744 0 5 0 0 0 4336 0 8 4294903361 3778 0 0 3 1266 202 208 98 4 1330 1360 98 2 530 91 1 530 595 41 4336 1330 3936 98 1 3970 3 1 3 4336 1330 4016 98 1 32 4336 1330 4064 98 1 98 2 7 3 4336 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 45 0 0 0 0 0 0 0 86 1 0 0 20 0 0 0] 98 0 1536 0 27 4162 4192 91 1098 4208 8 #fixedParentRight 1 4272 1 4304 41 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 3504 98 2 8 1142952260 1025 4784 0 482 8 4278190080 0 5 265030 4 ##(Smalltalk.Menu)  0 16 98 10 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 4946 1 4978 8 #bePlain 8 '&Plain' 1 1 0 0 0 4946 1 4978 8 #toggleBold 8 '&Bold' 1 1 0 0 0 4946 1 4978 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 4946 1 4978 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 5042 4097 4898 0 16 98 3 4946 1025 4978 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 4946 1025 4978 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 4946 1025 4978 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5042 4097 4946 1 4978 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 562 0 16 594 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 77 105 99 114 111 115 111 102 116 32 83 97 110 115 32 83 101 114 105 102 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 4784 0 8 1961168066 3778 0 0 11 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 80 0 70 1] 1266 202 208 98 6 1330 1360 98 2 530 1 41 530 685 637 4784 1330 8 #contextMenu: 98 1 4912 4784 1330 8 #text: 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Microsoft Sans Serif;}}
\viewkind4\uc1\pard\qc\f0\fs20 Type Rich Static text here.
\par }
' 4784 1330 3936 98 1 3970 3 1 3 4784 1330 4016 98 1 32 4784 1330 8 #resetCharFormat 1072 4784 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 86 1 0 0 82 1 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 41 1098 4208 8 #fixedParentBottom 1 234 256 98 6 3648 8 'section' 4336 8 'protocolName' 4784 8 'protocolDescription' 0 1266 202 208 98 1 1330 1360 98 2 530 9 45 530 685 645 3504 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 90 1 0 0 88 1 0 0] 98 3 3648 4336 4784 1536 0 27 898 8 'Chapter' 410 3520 98 15 0 3328 98 2 8 1140850688 131073 6560 0 0 0 5 0 0 0 6560 3586 234 240 98 6 410 3664 98 16 0 6560 98 2 8 1140916352 1025 6672 0 482 3744 0 5 0 0 0 6672 0 8 4294903361 3778 0 0 3 1266 202 208 98 4 1330 1360 98 2 530 91 3 530 595 41 6672 1330 3936 98 1 3970 3 1 3 6672 1330 4016 98 1 32 6672 1330 4064 98 1 98 2 7 3 6672 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 45 0 0 0 1 0 0 0 86 1 0 0 21 0 0 0] 98 0 1536 0 27 4162 4192 91 4752 1 4272 3 4304 41 410 4800 98 18 0 6560 98 2 8 1140920644 1025 7088 0 482 8 4278190080 0 5 4898 0 16 98 10 4946 1 4978 5008 8 '&Font...' 1 1 0 0 0 5042 4097 4946 1 4978 5104 8 '&Plain' 1 1 0 0 0 4946 1 4978 5168 8 '&Bold' 1 1 0 0 0 4946 1 4978 5232 8 '&Italic' 1 1 0 0 0 4946 1 4978 5296 8 '&Underlined' 1 1 0 0 0 5042 4097 4898 0 16 98 3 4946 1025 4978 5408 8 '&Left' 1 1 0 0 0 4946 1025 4978 5472 8 '&Centre' 1 1 0 0 0 4946 1025 4978 5536 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5042 4097 4946 1 4978 5632 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 7088 0 8 1961168066 3778 0 0 11 0 5778 8 #[0 0 0 0 0 0 0 0 80 0 70 1] 1266 202 208 98 6 1330 1360 98 2 530 1 41 530 685 637 7088 1330 5952 98 1 7184 7088 1330 6000 98 1 6034 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Verdana;}}
\viewkind4\uc1\pard\f0\fs16 
\par }
' 7088 1330 3936 98 1 3970 3 1 3 7088 1330 4016 98 1 32 7088 1330 6176 1072 7088 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 86 1 0 0 82 1 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 41 6256 1 410 3664 98 16 0 6560 98 2 8 1140916352 1025 8192 0 482 3744 0 5 0 0 0 8192 0 8 4294903361 3778 0 0 3 1266 202 208 98 4 1330 1360 98 2 530 1 3 530 91 41 8192 1330 3936 98 1 3970 3 1 3 8192 1330 4016 98 1 32 8192 1330 4064 98 1 98 2 7 3 8192 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 1 0 0 0 45 0 0 0 21 0 0 0] 98 0 1536 0 27 4162 4192 1 4240 91 4272 3 4304 41 234 256 98 6 6672 8 'chapterTitle' 7088 8 'chapterDescription' 8192 8 'chapterNumber' 0 1266 202 208 98 1 1330 1360 98 2 530 9 45 530 685 645 6560 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 90 1 0 0 88 1 0 0] 98 3 8192 6672 7088 1536 0 27 898 8 'Globals' 410 3520 98 15 0 3328 98 2 8 1140850688 131073 8880 0 0 0 5 0 0 0 8880 3586 234 240 98 4 410 3664 98 16 0 8880 98 2 8 1140916352 1025 8992 0 482 3744 0 5 0 0 0 8992 0 8 4294903361 3778 0 0 3 1266 202 208 98 4 1330 1360 98 2 530 1 1 530 685 41 8992 1330 3936 98 1 3970 3 1 3 8992 1330 4016 98 1 32 8992 1330 4064 98 1 98 2 7 3 8992 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 20 0 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 1 4304 41 410 4800 98 18 0 8880 98 2 8 1140920644 1025 9408 0 482 7168 0 5 4898 0 16 98 10 4946 1 4978 5008 8 '&Font...' 1 1 0 0 0 5042 4097 4946 1 4978 5104 8 '&Plain' 1 1 0 0 0 4946 1 4978 5168 8 '&Bold' 1 1 0 0 0 4946 1 4978 5232 8 '&Italic' 1 1 0 0 0 4946 1 4978 5296 8 '&Underlined' 1 1 0 0 0 5042 4097 4898 0 16 98 3 4946 1025 4978 5408 8 '&Left' 1 1 0 0 0 4946 1025 4978 5472 8 '&Centre' 1 1 0 0 0 4946 1025 4978 5536 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 5042 4097 4946 1 4978 5632 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 9408 0 8 1961168066 3778 0 0 11 0 5778 8 #[0 0 0 0 0 0 0 0 80 0 70 1] 1266 202 208 98 6 1330 1360 98 2 530 1 41 530 685 637 9408 1330 5952 98 1 9488 9408 1330 6000 98 1 6034 8 '{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fswiss\fprq2\fcharset0 Verdana;}}
\viewkind4\uc1\pard\f0\fs16 
\par }
' 9408 1330 3936 98 1 3970 3 1 3 9408 1330 4016 98 1 32 9408 1330 6176 1072 9408 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 20 0 0 0 86 1 0 0 82 1 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 41 6256 1 234 256 98 4 8992 8 'globalNames' 9408 8 'globalDescription' 0 1266 202 208 98 1 1330 1360 98 2 530 9 45 530 685 645 8880 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 90 1 0 0 88 1 0 0] 98 2 8992 9408 1536 0 27 898 8 'Conforms To' 410 3520 98 15 0 3328 98 2 8 1140850688 131073 10752 0 0 0 5 0 0 0 10752 3586 234 240 98 2 410 8 ##(Smalltalk.ListView)  98 30 0 10752 98 2 8 1140920397 1025 10864 1026 202 208 1072 0 1104 482 8 4278190080 0 5 0 0 0 10864 0 8 4294902841 1202 2624 98 0 0 2736 0 0 0 0 0 0 202 208 98 3 920646 5 ##(Smalltalk.ListViewColumn)  8 '#' 101 8 #left 1202 8 #section 98 0 1202 8 #<= 11184 0 0 10864 0 1 0 0 11090 8 'Name' 361 11136 1202 8 #name 11184 1202 11216 11184 0 0 10864 0 1 0 0 11090 8 'Chapter' 201 11136 1202 8 #chapterName 11184 1202 11216 11184 0 0 10864 0 1 0 0 8 #report 1072 0 2145 0 0 1266 202 208 98 2 1330 1360 98 2 530 1 1 530 685 677 10864 1330 6000 98 1 8 '#' 10864 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 82 1 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 1 6256 1 234 256 98 2 10864 8 'conformsToList' 0 1266 202 208 98 1 1330 1360 98 2 530 9 45 530 685 645 10752 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 90 1 0 0 88 1 0 0] 98 1 10864 1536 0 27 898 8 'Messages' 410 3520 98 15 0 3328 98 2 8 1140850688 131073 11872 0 0 0 7 0 0 0 11872 3586 234 240 98 2 410 10880 98 30 0 11872 98 2 8 1140920397 1025 11984 1026 202 208 1072 0 1104 482 8 4278190080 0 7 0 0 0 11984 0 8 4294902841 1202 2624 98 0 0 2736 0 0 0 0 0 0 202 208 98 3 11090 8 'Section' 161 11136 1202 11168 98 0 8 ##(Smalltalk.SortedCollection)  0 0 11984 0 1 0 0 11090 8 'Selector' 301 11136 1202 8 #selector 12240 1202 11216 12240 0 0 11984 0 1 0 0 11090 8 'Protocol' 161 11136 1202 8 #protocolName 11184 1202 11216 12240 0 0 11984 0 1 0 0 11392 1072 0 131169 0 0 1266 202 208 98 2 1330 1360 98 2 530 1 1 530 685 645 11984 1330 6000 98 1 8 'Section' 11984 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 86 1 0 0 66 1 0 0] 98 0 1536 0 27 4162 4192 1 4752 1 4272 1 6256 1 234 256 98 2 11984 8 'messageList' 0 1266 202 208 98 1 1330 1360 98 2 530 9 45 530 685 645 11872 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 22 0 0 0 90 1 0 0 88 1 0 0] 98 1 11984 1536 0 27 11872 234 256 98 10 3504 8 'protocolTab' 6560 8 'chapterTab' 10752 8 'conformsToTab' 8880 8 'globalsTab' 11872 8 'messagesTab' 0 410 2384 98 28 0 3328 98 2 8 1140916736 1 12976 1026 202 208 98 5 6544 3488 8864 10736 11856 0 1104 0 0 1 0 0 0 12976 0 8 4294903405 2514 0 0 2546 2 3 2576 2592 575230339 8 #[30 105 226 0 106] 2624 13104 7 257 0 2514 0 0 2546 2 3 2672 2688 579598755 8 #[30 105 226 0 106] 2720 13152 7 257 0 2736 0 0 0 0 0 2784 0 0 0 0 0 1266 202 208 98 3 1330 1360 98 2 530 1 1 530 701 697 12976 1330 2928 98 2 11 2960 12976 1330 3008 98 2 -1 1 12976 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 94 1 0 0 92 1 0 0] 98 0 1536 0 27 1266 202 208 98 1 1330 1360 98 2 530 485 1 530 701 697 3328 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 242 0 0 0 0 0 0 0 80 2 0 0 92 1 0 0] 98 6 6560 3504 8880 10752 11872 12976 1536 0 27 8 'rightGroup' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 2 4898 0 16 98 1 4946 1 4978 8 #exit 8 'E&xit' 17639 1 0 0 0 8 '&File' 0 134217729 0 0 10701 0 0 4898 0 16 98 1 4946 1 4978 8 #findMessage 8 '&Message' 1 1 0 0 0 8 '&Search' 0 134217729 0 0 10705 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1266 202 208 98 3 1330 1360 98 2 530 5359 21 530 1201 801 416 1330 6000 98 1 8 'Smalltalk 1998 ANSI Standard' 416 1330 8 #updateMenuBar 1072 416 1474 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 10 0 0 10 0 0 0 207 12 0 0 154 1 0 0] 98 3 720 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 14128 0 482 8 4278190080 0 519 0 0 0 14128 1266 202 208 98 1 1330 1360 98 2 530 475 1 530 11 697 14128 1474 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 237 0 0 0 0 0 0 0 242 0 0 0 92 1 0 0] 98 0 1536 0 27 3328 1536 0 27 )!

showSample

	AnsiStandard setDefaultToSample.
	self show.
! !
!AnsiShell class categoriesFor: #resource_Default_view!public!resources-views! !
!AnsiShell class categoriesFor: #showSample!public! !


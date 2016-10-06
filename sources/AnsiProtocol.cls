"Filed out from Dolphin Smalltalk 7"!

AnsiComponent subclass: #AnsiProtocol
	instanceVariableNames: 'chapter globals globalDescription isClass conformsTo description messages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiProtocol guid: (GUID fromString: '{85386FFC-A633-4765-A44C-BBC0D3ACE89A}')!
AnsiProtocol comment: ''!
!AnsiProtocol categoriesForClass!Unclassified! !
!AnsiProtocol methodsFor!

addMessagesTo: aDictionary

	| list |
	messages keysAndValuesDo: [:eachKey :eachValue | 
		list := aDictionary at: eachKey ifAbsentPut: [OrderedCollection new].
		list add: eachValue.
	].
!

chapter

	^chapter.
!

chapter: anAnsiChapter

	chapter := anAnsiChapter.
!

chapterName

	^chapter name.
!

conformsTo

	^conformsTo.
!

conformsToAll

	| list |
	list := conformsTo asOrderedCollection copy.
	conformsTo do: [:eachChild | 
		eachChild conformsToAll do: [:each | 
			(list includes: each) ifFalse: [list add: each].
		].
	].
	^list.

!

description

	^description.
!

globalDescription

	^globalDescription.
!

globalNames

	globals isEmpty ifTrue: [^'(none)'].
	globals size = 1 ifTrue: [^globals first].
	stream := WriteStream on: String new.
	globals do: [:each | stream nextPutAll: each; space].
	^stream contents.

!

globals

	^globals.
!

initialize: aList

	| first |
	isClass := false.
	globals := #().
	first := aList first.
	messages := aList copyFrom: 2 to: aList size.
	self 
		parseIntroduction: first;
		parseMessages;
		yourself.
!

isClass

	^isClass.
!

messages

	^messages.
!

parseIntroduction: aString

	| readStream writeStream line standardGlobals |
	readStream := ReadStream on: aString.
	section := readStream upTo: Character space.
	readStream upTo: $<.
	name := readStream upTo: $>.
	readStream nextLine; nextLine.
	conformsTo := readStream nextLine.
	line := readStream nextLine.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd or: [
			line := readStream nextLine.
			(line beginsWith: 'Messages') or: [line beginsWith: 'Standard Globals'].
		].
	] whileFalse: [
		writeStream nextPutAll: line.
		line last = $. 
			ifTrue: [writeStream cr; cr]
			ifFalse: [writeStream space].
	].
	description := writeStream contents trimBlanks.
	(line beginsWith: 'Messages') ifTrue: [^self].
	section = '5.3.1' ifTrue: [^self].		"we ignore the standard globals listed in Object"
	writeStream := WriteStream on: String new.
	[
		readStream atEnd or: [
			line := readStream nextLine.
			line beginsWith: 'Messages'.
		].
	] whileFalse: [
		writeStream nextPutAll: line.
		line last = $.
			ifTrue: [writeStream cr; cr]
			ifFalse: [writeStream space].
	].
	standardGlobals := writeStream contents.
	section = '5.6.8' ifTrue: [
		globals := Array 
			with: 'Float' 
			with: 'FloatD' 
			with: 'FloatE' 
			with: 'FloatQ'.
		globalDescription := standardGlobals.
		^self.].
	globals := Array with: standardGlobals subStrings first.
	globalDescription := (standardGlobals copyFrom: globals first size + 2 to: standardGlobals size) trimBlanks.
	isClass := name subStrings last = 'class'.
!

parseMessages

	| list message |
	list := OrderedCollection new.
	messages do: [:eachString | 
		message := AnsiMessage new
			initialize: eachString;
			protocol: self;
			yourself.
		message names do: [:each | 
			list add: (message copy
				name: each;
				yourself).
		].
	].
	messages := Dictionary new.
	list do: [:each | 
		messages 
			at: each selector 
			put: each.
	].!

updateConformsToWith: aDictionary

	stream := ReadStream on: conformsTo.
	conformsTo := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		| protocol |
		stream upTo: $<.
		protocol := stream upTo: $>.
		protocol = 'ANY' ifTrue: [
		] ifFalse: [
			conformsTo add: (aDictionary  at: protocol).
		].
	].
! !
!AnsiProtocol categoriesFor: #addMessagesTo:!public! !
!AnsiProtocol categoriesFor: #chapter!public! !
!AnsiProtocol categoriesFor: #chapter:!public! !
!AnsiProtocol categoriesFor: #chapterName!public! !
!AnsiProtocol categoriesFor: #conformsTo!public! !
!AnsiProtocol categoriesFor: #conformsToAll!public! !
!AnsiProtocol categoriesFor: #description!public! !
!AnsiProtocol categoriesFor: #globalDescription!public! !
!AnsiProtocol categoriesFor: #globalNames!public! !
!AnsiProtocol categoriesFor: #globals!public! !
!AnsiProtocol categoriesFor: #initialize:!public! !
!AnsiProtocol categoriesFor: #isClass!public! !
!AnsiProtocol categoriesFor: #messages!public! !
!AnsiProtocol categoriesFor: #parseIntroduction:!public! !
!AnsiProtocol categoriesFor: #parseMessages!public! !
!AnsiProtocol categoriesFor: #updateConformsToWith:!public! !


"Filed out from Dolphin Smalltalk 7"!

AnsiComponent subclass: #AnsiChapter
	instanceVariableNames: 'introduction protocols'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiChapter guid: (GUID fromString: '{9A5840B0-DA0A-4CF6-A779-501DC7555B98}')!
AnsiChapter comment: ''!
!AnsiChapter categoriesForClass!Unclassified! !
!AnsiChapter methodsFor!

buildProtocols

	| list |
	self parseIntroduction.
	list := protocols.
	protocols := OrderedCollection new.
	list do: [:each | 
		(each subStrings first occurrencesOf: $.) = 2 ifTrue: [
			protocols add: OrderedCollection new.
		].
		protocols last add: each.
	].
	protocols := protocols collect: [:each | 
		AnsiProtocol new
			initialize: each;
			chapter: self;
			yourself.
	].
!

description

	^introduction.
!

initialize: aString

	| line |
	stream := ReadStream on: aString.
	protocols := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		line := stream nextLine.
		(line beginsWith: '5.') ifTrue: [
			protocols add: (WriteStream on: String new).
		].
		protocols last nextPutAll: line; cr.
	].
	protocols := protocols collect: [:each | each contents].
	introduction := protocols removeFirst.
	self buildProtocols.
	stream := nil.!

parseIntroduction

	| lines index |
	stream := ReadStream on: introduction.
	lines := OrderedCollection new.
	[stream atEnd] whileFalse: [lines add: stream nextLine].
	[
		lines last first = $<.
	] whileTrue: [
		lines removeLast.
	].
	name := lines removeFirst.
	index := name indexOf: Character space.
	section := name copyFrom: 1 to: index - 1.
	name := name copyFrom: index + 1 to: name size - 10.
	stream := WriteStream on: String new.
	lines do: [:each | 
		stream nextPutAll: each.
		each last = $.
			ifTrue: [stream cr; cr]
			ifFalse: [stream space].
	].
	introduction := stream contents.
!

protocols

	^protocols.
! !
!AnsiChapter categoriesFor: #buildProtocols!public! !
!AnsiChapter categoriesFor: #description!public! !
!AnsiChapter categoriesFor: #initialize:!public! !
!AnsiChapter categoriesFor: #parseIntroduction!public! !
!AnsiChapter categoriesFor: #protocols!public! !


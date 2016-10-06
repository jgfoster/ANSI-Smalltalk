"Filed out from Dolphin Smalltalk 7"!

Object subclass: #AnsiStandard
	instanceVariableNames: 'stream chapters protocols globals classes messages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
AnsiStandard guid: (GUID fromString: '{FFA619B7-20B5-4335-9F41-D388B1BA2219}')!
AnsiStandard comment: 'AnsiStandard sample.
AnsiStandard writeSample.
AnsiShell show.'!
!AnsiStandard categoriesForClass!Unclassified! !
!AnsiStandard methodsFor!

buildChapter

	| line |
	line := self nextLine.
	((line beginsWith: '5.') and: [(line subStrings first occurrencesOf: $.) = 1]) ifTrue: [
		chapters add: (WriteStream on: String new).
	].
	chapters last nextPutAll: line; cr.
!

chapters

	^chapters.
!

classes

	^classes.
!

fileOutToPath: aString

	| path file |
	path := aString.
	(path asUppercase endsWith: '.STB') ifFalse: [
		path := path , '.STB'.
	].
	file := FileStream write: path text: false.
	(STBOutFiler on: file) nextPut: self.
	file close.
!

globals

	^globals.
!

initialize: aStream

	stream := aStream.
	self
		readChapters;
		parseChapters;
		loadProtocols;
		loadGlobals;
		loadClasses;
		loadMessages;
		updateConformsTo;
		yourself.
	stream := nil.!

loadClasses

	classes := Dictionary new.
	protocols do: [:each |
		each isClass ifTrue: [
			classes
				at: each globals first
				put: each.
		].
	].
!

loadGlobals

	globals := Dictionary new.
	protocols do: [:eachProtocol | 
		eachProtocol globals do: [:eachName |
			globals 
				at: eachName
				put: eachProtocol.
		].
	].

!

loadMessages

	messages := Dictionary new.
	protocols do: [:each | each addMessagesTo: messages].
!

loadProtocols

	protocols := Dictionary new.
	chapters do: [:eachChapter | 
		eachChapter protocols do: [:each |
			protocols
				at: each name
				put: each.
		].
	].
!

messages

	^messages.
!

nextLine

	| string |
	string := stream nextLine.
	string = 'ANSI NCITS 319-1998' ifTrue: [
		stream nextLine. 
		^self nextLine.
	].
	^string.
!

parseChapters

	chapters := chapters collect: [:each | 
		AnsiChapter new
			initialize: each;
			yourself.
	].
!

protocols

	^protocols.
!

readChapters

	chapters := OrderedCollection new.
	[
		stream atEnd not.
	] whileTrue: [
		self buildChapter.
	].
	chapters := chapters collect: [:each | each contents].
!

updateConformsTo

	protocols do: [:each | 
		each updateConformsToWith: protocols.
	].
!

write

	self fileOutToPath: 'C:\Documents and Settings\James\My Documents\Dolphin Smalltalk X6\ANSI.STB'.
! !
!AnsiStandard categoriesFor: #buildChapter!public! !
!AnsiStandard categoriesFor: #chapters!public! !
!AnsiStandard categoriesFor: #classes!public! !
!AnsiStandard categoriesFor: #fileOutToPath:!public! !
!AnsiStandard categoriesFor: #globals!public! !
!AnsiStandard categoriesFor: #initialize:!public! !
!AnsiStandard categoriesFor: #loadClasses!public! !
!AnsiStandard categoriesFor: #loadGlobals!public! !
!AnsiStandard categoriesFor: #loadMessages!public! !
!AnsiStandard categoriesFor: #loadProtocols!public! !
!AnsiStandard categoriesFor: #messages!public! !
!AnsiStandard categoriesFor: #nextLine!public! !
!AnsiStandard categoriesFor: #parseChapters!public! !
!AnsiStandard categoriesFor: #protocols!public! !
!AnsiStandard categoriesFor: #readChapters!public! !
!AnsiStandard categoriesFor: #updateConformsTo!public! !
!AnsiStandard categoriesFor: #write!public! !

!AnsiStandard class methodsFor!

buildDefaultFromPath: aString

	self default: (self buildFromPath: aString).
!

buildFromPath: aString

	| stream instance |
	stream := FileStream read: aString.
	[
		instance := self new 
			initialize: stream;
			yourself.
	] ensure: [
		stream close.
	].
	^instance.
!

clearDefault

	default := nil.
!

default

	^default.
!

default: anObject

	default := anObject.
!

readDefaultFromPath: aString

	self default: (self readFromPath: aString).
!

readFromPath: aString
"
	AnsiStandard readFromPath:  'M:\Documents\Dolphin\ANSI\ANSI.STB'.
"
	| stream stb instance |
	stream := FileStream read: aString text: false.
	stb := STBInFiler on: stream.
	instance := stb next.
	stream close.
	^instance.
!

sample

	^self buildFromPath: self samplePath.
!

samplePath

	^'C:\Documents and Settings\James\My Documents\Dolphin\ANSI\ANSI.txt'.
!

setDefaultToSample

	self default: self sample.
!

writeSample

	| path |
	path := 'C:\Documents and Settings\James\My Documents\Dolphin\ANSI\ANSI.STB'.
	self sample fileOutToPath: path.
! !
!AnsiStandard class categoriesFor: #buildDefaultFromPath:!public! !
!AnsiStandard class categoriesFor: #buildFromPath:!public! !
!AnsiStandard class categoriesFor: #clearDefault!public! !
!AnsiStandard class categoriesFor: #default!public! !
!AnsiStandard class categoriesFor: #default:!public! !
!AnsiStandard class categoriesFor: #readDefaultFromPath:!public! !
!AnsiStandard class categoriesFor: #readFromPath:!public! !
!AnsiStandard class categoriesFor: #sample!public! !
!AnsiStandard class categoriesFor: #samplePath!public! !
!AnsiStandard class categoriesFor: #setDefaultToSample!public! !
!AnsiStandard class categoriesFor: #writeSample!public! !


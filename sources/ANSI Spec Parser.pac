| package |
package := Package name: 'ANSI Spec Parser'.
package paxVersion: 1;
	basicComment: ''.

package basicPackageVersion: '0.009'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAzIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAAEAAAAEFOU0kgU3BlYyBQYXJzZXJS
AAAAEAAAAFJ1bnRpbWVcQU5TSS5leGWaAAAAUgAAABAAAABEb2xwaGluIE1WUCBCYXNlUgAAABUA
AABSdW50aW1lU2Vzc2lvbk1hbmFnZXLvvyUAUgAAAD4AAABBbnNpU2hlbGwgYWxsSW5zdGFuY2Vz
IGlzRW1wdHkgaWZUcnVlOiBbQW5zaVNoZWxsIHNob3dTYW1wbGVdLgAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAA==').

package classNames
	add: #AnsiChapter;
	add: #AnsiComponent;
	add: #AnsiMessage;
	add: #AnsiMessageShell;
	add: #AnsiParameter;
	add: #AnsiProtocol;
	add: #AnsiShell;
	add: #AnsiStandard;
	add: #GsAnsi;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Rich Text Presenter';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\..\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	yourself).

package!

"Class Definitions"!

Object subclass: #AnsiComponent
	instanceVariableNames: 'stream section sectionNumber name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #AnsiStandard
	instanceVariableNames: 'stream chapters protocols globals classes messages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
Object subclass: #GsAnsi
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiComponent subclass: #AnsiChapter
	instanceVariableNames: 'introduction protocols'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiComponent subclass: #AnsiMessage
	instanceVariableNames: 'protocol selector names synopsis definition parameters returnValue errors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiComponent subclass: #AnsiParameter
	instanceVariableNames: 'protocols isCaptured'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AnsiComponent subclass: #AnsiProtocol
	instanceVariableNames: 'chapter globals globalDescription isClass conformsTo description messages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AnsiMessageShell
	instanceVariableNames: 'definitionPresenter errorPresenter namePresenter parameterListPresenter returnValuePresenter synopsisPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AnsiShell
	instanceVariableNames: 'chapterDescriptionPresenter chapterNumberPresenter chapterTitlePresenter classListPresenter conformsToListPresenter globalDescriptionPresenter globalListPresenter globalNamesPresenter messageListPresenter protocolDescriptionPresenter protocolListPresenter protocolNamePresenter sectionPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AnsiComponent guid: (GUID fromString: '{756D2F40-4863-4ECA-8DCC-602A0DB66D85}')!
AnsiComponent comment: ''!
!AnsiComponent categoriesForClass!Unclassified! !
!AnsiComponent methodsFor!

<= anAnsiComponent

	^self sectionNumber <= anAnsiComponent sectionNumber.
!

name

	^name.
!

printOn: aStream

	aStream
		nextPutAll: section; space;
		nextPutAll: name;
		yourself.
!

section
	^section!

sectionNumber

	sectionNumber isNil ifTrue: [
		sectionNumber := 0.
		(section subStrings: $.) do: [:each | 
			sectionNumber := sectionNumber * 100 + each asNumber.
		].
	].
	^sectionNumber.
! !
!AnsiComponent categoriesFor: #<=!public! !
!AnsiComponent categoriesFor: #name!public! !
!AnsiComponent categoriesFor: #printOn:!public! !
!AnsiComponent categoriesFor: #section!accessing!public! !
!AnsiComponent categoriesFor: #sectionNumber!public! !

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

GsAnsi guid: (GUID fromString: '{8FB0BE87-DC6B-46EB-BB2A-77101CEF636A}')!
GsAnsi comment: ''!
!GsAnsi categoriesForClass!Kernel-Objects! !
!GsAnsi class methodsFor!

script

"
AnsiShell show.
"
| children parents std x |
std := AnsiStandard readFromPath:  'M:\Documents\Dolphin\ANSI\ANSI.STB'.
children := Dictionary new.
std protocols do: [:each | 
	1 < (x := each conformsTo) size ifTrue: [
		children at: each put: each conformsTo.
	].
].
children.
parents := Dictionary new.
children keysAndValuesDo: [:eachChild :eachParents | 
	eachParents do: [:eachParent |
		(parents at: eachParent ifAbsentPut: [OrderedCollection new]) add: eachChild.
	].
]. 
parents.
! !
!GsAnsi class categoriesFor: #script!public! !

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

AnsiMessage guid: (GUID fromString: '{484089F7-3B52-42B2-BB07-A6BA60FC6C0C}')!
AnsiMessage comment: ''!
!AnsiMessage categoriesForClass!Unclassified! !
!AnsiMessage methodsFor!

definition
	^definition!

errors
	^errors!

initialize: aString

	stream := ReadStream on: aString.
	section := stream upTo: Character space.
	self  readNames.
	stream := nil.
!

name: aString

	| list |
	name := aString.
	stream := WriteStream on: String new.
	list := name subStrings.
	1 to: list size by: 2 do: [:i | 
		stream nextPutAll: (list at: i).
	].
	selector := stream contents.
!

names

	^names.
!

parameters
	^parameters!

protocol

	^protocol.
!

protocol: anAnsiProtocol

	protocol := anAnsiProtocol.
!

protocolName

	^protocol name.
!

readDefinition

	| writeStream line |
	writeStream := WriteStream on: String new.
	stream nextLine.
	[
		stream atEnd not.
	] whileTrue: [
		line := stream nextLine.
		(line beginsWith: 'Parameters') 		ifTrue: [definition := writeStream contents. ^self readParameters	].
		(line beginsWith: 'Return Value') 	ifTrue: [definition := writeStream contents. ^self readReturnValue	].
		(line beginsWith: 'Errors') 			ifTrue: [definition := writeStream contents. ^self readErrors		].
		writeStream nextPutAll: line; cr.
		(line beginsWith: 'Refinement')		 ifTrue: [writeStream := WriteStream on: String new				].
	].
	self error: 'Unexpected end-of-file'.
!

readErrors

	| writeStream |
	writeStream := WriteStream on: String new.
	[
		stream atEnd not.
	] whileTrue: [
		writeStream nextPutAll: stream nextLine; cr.
	].
	errors := writeStream contents trimBlanks.
!

readNames

	| word |
	names := OrderedCollection new.
	[
		stream atEnd ifTrue: [self halt].
		word := stream nextWord.
		(word beginsWith: 'Synopsis') 		ifTrue: [name := names first. ^self readSynopsis		].
		(word beginsWith: 'Definition') 		ifTrue: [name := names first. ^self readDefinition	].
		(word beginsWith: 'Parameters') 	ifTrue: [name := names first. ^self readParameters	].
		(word beginsWith: 'Return Value') 	ifTrue: [name := names first. ^self readReturnValue	].
		(word beginsWith: 'Errors') 		ifTrue: [name := names first. ^self readErrors		].
		true.
	] whileTrue: [
		(word beginsWith: 'Message') ifTrue: [
			(stream peek = $R) ifTrue: [
				((word := stream nextWord) beginsWith: 'Refinement') ifFalse: [self halt].
			].
			names add: stream nextLine trimBlanks.
		] ifFalse: [
			names add: (names removeLast , ' ' , word , ' ' , stream nextLine) trimBlanks.
		].
	].
	self error: 'should not get here!!'.

!

readParameters

	| line |
	parameters := OrderedCollection new.
	[
		stream atEnd ifTrue: [self halt].
		line := stream nextLine.
		line beginsWith: 'Return Value'.
	] whileFalse: [
		parameters add: (AnsiParameter new initialize: line; yourself).
	].
	1 to: parameters size do: [:i | 
		(parameters at: i) section: i printString.
	].
	self readReturnValue.
!

readReturnValue

	| writeStream line |
	writeStream := WriteStream on: String new.
	[
		stream atEnd ifTrue: [returnValue := writeStream contents trimBlanks. ^self].
		line := stream nextLine.
		line beginsWith: 'Errors'.
	] whileFalse: [
		writeStream nextPutAll: line; cr.
	].
	returnValue := writeStream contents trimBlanks.
	self readErrors.
!

readSynopsis

	| writeStream word |
	writeStream := WriteStream on: String new.
	[
		stream atEnd ifTrue: [self halt].
		word := stream nextWord.
		(word beginsWith: 'Definition') or: [word beginsWith: 'Refinement'].
	] whileFalse: [
		writeStream 
			nextPutAll: word;
			space;
			nextPutAll: stream nextLine;
			space.
	].
	synopsis := writeStream contents trimBlanks.
	self readDefinition.
!

refines

	^'???'.
!

returnValue
	^returnValue!

selector
	^selector!

synopsis
	^synopsis!

upToLine: aStringOrArray

	| writeStream line |
	writeStream := WriteStream on: String new.
	[
		stream atEnd or: [
			line := stream nextLine.
			(aStringOrArray isKindOf: String) 
				ifTrue: [line beginsWith: aStringOrArray]
				ifFalse: [(line beginsWith: aStringOrArray first) or: [line beginsWith: aStringOrArray last]].
		].
	] whileFalse: [
		writeStream nextPutAll: line; cr.
	].
	^writeStream contents trimBlanks.
! !
!AnsiMessage categoriesFor: #definition!accessing!public! !
!AnsiMessage categoriesFor: #errors!accessing!public! !
!AnsiMessage categoriesFor: #initialize:!public! !
!AnsiMessage categoriesFor: #name:!public! !
!AnsiMessage categoriesFor: #names!public! !
!AnsiMessage categoriesFor: #parameters!accessing!public! !
!AnsiMessage categoriesFor: #protocol!public! !
!AnsiMessage categoriesFor: #protocol:!public! !
!AnsiMessage categoriesFor: #protocolName!public! !
!AnsiMessage categoriesFor: #readDefinition!public! !
!AnsiMessage categoriesFor: #readErrors!public! !
!AnsiMessage categoriesFor: #readNames!public! !
!AnsiMessage categoriesFor: #readParameters!public! !
!AnsiMessage categoriesFor: #readReturnValue!public! !
!AnsiMessage categoriesFor: #readSynopsis!public! !
!AnsiMessage categoriesFor: #refines!public! !
!AnsiMessage categoriesFor: #returnValue!accessing!public! !
!AnsiMessage categoriesFor: #selector!public! !
!AnsiMessage categoriesFor: #synopsis!accessing!public! !
!AnsiMessage categoriesFor: #upToLine:!public! !

AnsiParameter guid: (GUID fromString: '{3A448A3C-4554-4DAD-9B24-8C7CCF62530F}')!
AnsiParameter comment: ''!
!AnsiParameter categoriesForClass!Unclassified! !
!AnsiParameter methodsFor!

initialize: aString

	| list |
	list := aString subStrings.
	name := list first.
	isCaptured := #(true false nil) at: (#('captured' 'uncaptured' 'unspecified') indexOf: list last).
	list size = 3 ifTrue: [
		protocols := Array with: (list at: 2).
	] ifFalse: [list size = 5 ifTrue: [
		protocols := Array with: (list at: 2) with: (list at: 4).
	] ifFalse: [
		self halt.
	]].
!

isCaptured

	^isCaptured.
!

isCapturedString

	isCaptured isNil ifTrue: [^'???'].
	^isCaptured
		ifTrue: ['Yes']
		ifFalse: ['No'].

!

section: aString

	section := aString.
!

typesString

	^protocols size = 1
		ifTrue: [protocols first]
		ifFalse: [protocols first , ' | ' , protocols last].
! !
!AnsiParameter categoriesFor: #initialize:!public! !
!AnsiParameter categoriesFor: #isCaptured!public! !
!AnsiParameter categoriesFor: #isCapturedString!public! !
!AnsiParameter categoriesFor: #section:!public! !
!AnsiParameter categoriesFor: #typesString!public! !

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

"Binary Globals"!


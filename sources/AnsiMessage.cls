"Filed out from Dolphin Smalltalk 7"!

AnsiComponent subclass: #AnsiMessage
	instanceVariableNames: 'protocol selector names synopsis definition parameters returnValue errors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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


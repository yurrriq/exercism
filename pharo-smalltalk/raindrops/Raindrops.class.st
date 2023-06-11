Class {
	#name : #Raindrops,
	#superclass : #Object,
	#category : #'Exercise@Raindrops'
}

{ #category : #exercism }
Raindrops >> convertNumber: anInteger [
	^ String streamContents: [ :sounds |
		self raindrops keysAndValuesDo: [ :factor :sound |
		(anInteger isDivisibleBy: factor)
			ifTrue: [ sounds << sound ] ].
		(sounds isEmpty)
			ifTrue: [ sounds << anInteger asString ] ]
]

{ #category : #exercism }
Raindrops >> raindrops [
	^ { 3 -> 'Pling'. 5 -> 'Plang'. 7 -> 'Plong'. } asOrderedDictionary
]

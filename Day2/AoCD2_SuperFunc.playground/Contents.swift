//: Playground - noun: a place where people can play

import Foundation

enum AdventError: Error {
    case InputPathFailed
}

func area(sides: AnyCollection<Int>) -> Int {
    if let head = sides.first {
        let tail = sides.dropFirst()
        let x = tail.map {2 * $0 * head}.reduce(0, +)
        return x + area(sides: tail)
    }
    else {
        return 0
    }
}

func areaOfSmallestSide(sides: AnyCollection<Int>) -> Int {
    if let min = sides.sorted().first, let min2 = sides.sorted().dropFirst().first {
        return min * min2
    }
    else {
        return 0
    }
}

// Start

guard let filePath = Bundle.main.path(forResource: "input", ofType: nil) else {
    throw AdventError.InputPathFailed
}

let fileString: String = try NSString(contentsOfFile: filePath, encoding: String.Encoding.utf8.rawValue) as String

let lines = fileString.characters.split(separator: "\n")

//let answer = lines.map { $0.split(separator: "x") }.map { $0.flatMap { Int(String($0)) } }.map {area(sides: AnyCollection($0)) + areaOfSmallestSide(sides: AnyCollection($0)) }.reduce(0, +)

let arrayOfSides = lines.map { $0.split(separator: "x") }.map { $0.flatMap { Int(String($0)) } }
let arrayOfAreas = arrayOfSides.map {area(sides: AnyCollection($0)) + areaOfSmallestSide(sides: AnyCollection($0)) }
let answer = arrayOfAreas.reduce(0, +)

print(answer)

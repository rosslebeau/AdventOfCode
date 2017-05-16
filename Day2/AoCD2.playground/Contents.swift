//: Playground - noun: a place where people can play

import Foundation

enum AdventError: Error {
    case InputPathFailed
}

func wrappingPaperNeeded(length: Int, width: Int, height: Int) -> Int {
    return areaOfBox(length: length, width: width, height: height) + areaOfSmallestSide(length: length, width: width, height: height)
}

func areaOfBox(length: Int, width: Int, height: Int) -> Int {
    return (2 * length * width) + (2 * width * height) + (2 * height * length)
}

func areaOfSmallestSide(length: Int, width: Int, height: Int) -> Int {
    return length * width * height / max(max(length, width), height)
}

func boxDimensions(fromString: String) -> (Int, Int, Int)? {
    let dimensions = fromString.characters.split(separator: "x").flatMap { Int(String($0)) }
    if let length = dimensions.first, let width = dimensions.dropFirst().first, let height = dimensions.dropFirst().dropFirst().first {
        return (length, width, height)
    }
    else {
        return nil
    }
}

// Start

guard let filePath = Bundle.main.path(forResource: "input", ofType: nil) else {
    throw AdventError.InputPathFailed
}

let fileString: String = try NSString(contentsOfFile: filePath, encoding: String.Encoding.utf8.rawValue) as String

let lines = fileString.characters.split(separator: "\n").map(String.init)
let answer = lines.flatMap(boxDimensions).map { wrappingPaperNeeded(length: $0.0, width: $0.1, height: $0.2) }.reduce(0, +)

print(answer)

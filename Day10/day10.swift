import Cocoa

func lookAndSay(input: String) -> String {
    var newString = ""
    guard var currentChar = input.characters.first else {
        return ""
    }
    
    var count = 1
    for char in input.characters[Range(start: input.characters.startIndex.successor(), end: input.characters.endIndex)] {
        if char == currentChar {
            count++
            continue
        }
        else {
            newString = newString + String(count)
            newString = newString + String(currentChar)
            currentChar = char
            count = 1
        }
    }
    
    newString = newString + String(count)
    newString = newString + String(currentChar)
    
    return newString
}

do {
    let input = "1113122113"
    
    let forty = [Int](count: 40, repeatedValue: 1)
    let part1 = forty.reduce(input, combine: {(s, x) in lookAndSay(s)})
    print(part1.characters.count)
    
    let ten = [Int](count: 10, repeatedValue: 1)
    let part2 = ten.reduce(part1, combine: {(s, x) in lookAndSay(s)})
    print(part2.characters.count)
    
}
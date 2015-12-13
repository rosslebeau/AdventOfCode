import Cocoa

enum AdventError: ErrorType {
    case WrongChar
}

let alphabet = "abcdefghijklmnopqrstuvwxyz".characters

func cycleChar(oldChar: Character) throws -> Character {
    
    
    guard let oldCharIndex = alphabet.indexOf(oldChar) else {
        throw AdventError.WrongChar
    }
    
    if oldCharIndex == alphabet.endIndex.predecessor() {
        return "a"
    }
    else {
        return alphabet[oldCharIndex.successor()]
    }
}

func incrementPassword(oldPass: String) throws -> String {
    let initial = oldPass.characters[Range(start: oldPass.characters.startIndex, end: oldPass.characters.endIndex.predecessor())]
    let tail = oldPass.characters[oldPass.characters.endIndex.predecessor()]
    if String(tail) == "z" {
        if initial.isEmpty {
            return "aa"
        }
        else {
            return try incrementPassword(String(initial)) + "a"
        }
    }
    else {
        return try String(initial) + String(cycleChar(tail))
    }
}

func isBannedLetter(letter: Character) -> Bool {
    if letter == "i" {
        return true
    }
    
    if letter == "i" {
        return true
    }
    
    if letter == "i" {
        return true
    }
    
    return false
}

// Moved everything into single fuction so I only have to iterate through each candidate string once
func isValidPassword(pass: String) throws -> Bool {
    let passChars = pass.characters
    
    var has3Straight = false
    var strightCount = 1
    
    var has2Repeat = false
    var numRepeats = 0
    var curValidForRepeat = true
    
    var prevChar = passChars[passChars.startIndex]
    
    if isBannedLetter(prevChar) {
        return false
    }
    
    for curChar in passChars[Range(start: passChars.startIndex.successor(), end: passChars.endIndex)] {
        if isBannedLetter(curChar) {
            return false
        }
        
        if curValidForRepeat {
            if !has2Repeat && curChar == prevChar {
                numRepeats = numRepeats + 1
                if numRepeats == 2 {
                    has2Repeat = true
                }
                else {
                    curValidForRepeat = false
                }
            }
        }
        else {
            curValidForRepeat = true
        }
        
        if !has3Straight {
            if curChar == "a" {
                strightCount = 1
                continue
            }
            
            guard let curCharIndex = alphabet.indexOf(curChar), let prevCharIndex = alphabet.indexOf((prevChar)) else {
                throw AdventError.WrongChar
            }
            
            if curCharIndex == prevCharIndex.successor() {
                strightCount = strightCount + 1
                if strightCount == 3 {
                    has3Straight = true
                }
            }
            else {
                strightCount = 1
            }
        }
        
        prevChar = curChar
    }
    
    return has3Straight && has2Repeat
}

func nextValidPassword(oldPass: String) throws -> String {
    let incremented = try incrementPassword(oldPass)
    
    if try isValidPassword(incremented) {
        return incremented
    }
    else {
        return try nextValidPassword(incremented)
    }
}

do {
    let input = "cqjxjnds"
    try print(nextValidPassword(input))
}
catch AdventError.WrongChar {
    print("Password had invalid character")
}

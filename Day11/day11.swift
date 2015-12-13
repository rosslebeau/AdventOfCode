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
    var straightCount = 1
    
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
                straightCount = 1
            }
            else {
                guard let curCharIndex = alphabet.indexOf(curChar), let prevCharIndex = alphabet.indexOf((prevChar)) else {
                    throw AdventError.WrongChar
                }
                
                if curCharIndex == prevCharIndex.successor() {
                    straightCount = straightCount + 1
                    if straightCount == 3 {
                        has3Straight = true
                    }
                }
                else {
                    straightCount = 1
                }
            }
        }
        
        prevChar = curChar
    }
    
    return has3Straight && has2Repeat
}

func nextValidPassword(oldPass: String) throws -> String {
    var curPass = try incrementPassword(oldPass)
    var found = false
    
    while found == false {
        if try isValidPassword(curPass) {
            found = true
        }
        else {
            curPass = try incrementPassword(curPass)
        }
    }
    
    return curPass
}

do {
    let input = "cqjxjnds"
    
    let nextValidPass1 = try nextValidPassword(input)
    print("Part 1: " + nextValidPass1)
    
    let nextValidPass2 = try nextValidPassword(nextValidPass1)
    print("Part 2: " + nextValidPass2)
}
catch AdventError.WrongChar {
    print("Password had invalid character")
}
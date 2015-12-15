import Cocoa

enum AdventError: ErrorType {
    case InputPathFailed
    case FileError
    case JSONError
}

func sumAllNumbers(json: AnyObject, acc: Int) throws -> Int {
    if json.isKindOfClass(NSNumber) {
        let num = json as! NSNumber
        return acc + num.integerValue
    }
    else if json.isKindOfClass(NSDictionary) {
        let dict = json as! NSDictionary
        return try dict.allKeys.map({key -> Int in
            guard let val = dict.objectForKey(key) else {
                throw AdventError.JSONError
            }
            return try sumAllNumbers(val, acc: 0)
        })
        .reduce(acc, combine: +)
    }
    else if json.isKindOfClass(NSArray) {
        let arr = json as! NSArray
        return try arr.reduce(0, combine: {try sumAllNumbers($1, acc: 0) + $0}) + acc
    }
    else {
        return 0
    }
}

do {
    guard let filePath = NSBundle.mainBundle().pathForResource("input", ofType: nil) else {
        throw AdventError.InputPathFailed
    }
    
    guard let data = NSFileManager.defaultManager().contentsAtPath(filePath) else {
        throw AdventError.FileError
    }
    
    let json = try NSJSONSerialization.JSONObjectWithData(data, options: NSJSONReadingOptions.MutableContainers)
    
    try print(sumAllNumbers(json, acc: 0))
}
catch AdventError.InputPathFailed {
    print("Couldn't find file")
}
catch AdventError.FileError {
    print("Couldn't open the file")
}
catch AdventError.JSONError {
    print("JSON error")
}

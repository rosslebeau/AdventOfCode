import Cocoa

enum AdventError: ErrorType {
    case InputPathFailed
    case InvalidInput(input: String)
    case PathNotFound(cityA: String, cityB: String)
    case EmptyDestination
    case NotEnoughCities
}

// This assumes distance(a, b) == distance(b, a) for all cities a and b
// and use this to fill in entries for both directions
func cityPathsEqual(input: [NSString]) throws -> [String: [String: Int]] {
    var cities = [String: [String: Int]]()
    
    for line in input {
        let componentsRegex = try NSRegularExpression(pattern: "(.+) to (.+) = ([0-9]+)", options:NSRegularExpressionOptions(rawValue: 0))
        let matchedComponents = componentsRegex.matchesInString(line as String, options: NSMatchingOptions(rawValue: 0), range: NSMakeRange(0, line.length))
        
        if matchedComponents.count == 1 && matchedComponents[0].numberOfRanges == 4 {
            let a = line.substringWithRange(matchedComponents[0].rangeAtIndex(1)) as String
            let b = line.substringWithRange(matchedComponents[0].rangeAtIndex(2)) as String
            guard let d = Int(line.substringWithRange(matchedComponents[0].rangeAtIndex(3)) as String) else {
                throw AdventError.InvalidInput(input: line as String)
            }
            
            addPathsToCities(a, cityB: b, distance: d, currentPaths: &cities)
        }
        else {
            
            throw AdventError.InvalidInput(input: line as String)
        }
    }
    
    return cities
}

func addPathsToCities(cityA: String, cityB: String, distance: Int, inout currentPaths: [String: [String: Int]]) {
    addPathToCities(cityA, cityB: cityB, distance: distance, currentPaths: &currentPaths)
    addPathToCities(cityB, cityB: cityA, distance: distance, currentPaths: &currentPaths)
}

func addPathToCities(cityA: String, cityB: String, distance: Int, inout currentPaths: [String: [String: Int]]) {
    var cityAPaths: [String: Int]
    if let existingPaths = currentPaths[cityA] {
        cityAPaths = existingPaths
    }
    else {
        cityAPaths = [String: Int]()
    }
    
    cityAPaths[cityB] = distance
    currentPaths[cityA] = cityAPaths
}

func pathBetweenCities(cityA: String, cityB: String, paths: [String: [String: Int]]) throws -> Int {
    guard let startCityPaths = paths[cityA], let endCityDistance = startCityPaths[cityB] else {
        throw AdventError.PathNotFound(cityA: cityA, cityB: cityB)
    }
    
    return endCityDistance
}

// Held-Karp algorithm
// startCity should not be in cities, but endCity should
func getMinimumDistance(startCity: String, cities: [String], endCity: String, paths: [String: [String: Int]]) throws -> Int {
    if cities.isEmpty {
        throw AdventError.EmptyDestination
    }
    
    if cities.count == 1 {
        return try pathBetweenCities(startCity, cityB: endCity, paths: paths)
    }
    else {
        let citiesMinusEndCity = cities.filter({city in city != endCity})
        let minDistanceToOtherCities = try citiesMinusEndCity.map({newEndCity in
            try getMinimumDistance(startCity, cities: citiesMinusEndCity, endCity: newEndCity, paths: paths) +
            pathBetweenCities(newEndCity, cityB: endCity, paths: paths)
        }).reduce(Int.max, combine: {min($0, $1)})
        return minDistanceToOtherCities
    }
}

func getMaximumDistance(startCity: String, cities: [String], endCity: String, paths: [String: [String: Int]]) throws -> Int {
    if cities.isEmpty {
        throw AdventError.EmptyDestination
    }
    
    if cities.count == 1 {
        return try pathBetweenCities(startCity, cityB: endCity, paths: paths)
    }
    else {
        let citiesMinusEndCity = cities.filter({city in city != endCity})
        let maxDistanceToOtherCities = try citiesMinusEndCity.map({newEndCity in
            try getMaximumDistance(startCity, cities: citiesMinusEndCity, endCity: newEndCity, paths: paths) +
                pathBetweenCities(newEndCity, cityB: endCity, paths: paths)
        }).reduce(0, combine: {max($0, $1)})
        return maxDistanceToOtherCities
    }
}

do {
    guard let filePath = NSBundle.mainBundle().pathForResource("input", ofType: nil) else {
        throw AdventError.InputPathFailed
    }
    
    let fileContent = try NSString(contentsOfFile: filePath, encoding: NSUTF8StringEncoding)
    
    let lines = fileContent.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet()).filter{!$0.isEmpty}
    
    let cityPaths = try cityPathsEqual(lines)
    let cities = Array(cityPaths.keys)
    
    if cities.count < 2 {
        throw AdventError.NotEnoughCities
    }
    
    let minDistance = try cities.map({startCity -> Int in
        let endCities = cities.filter(({city in city != startCity}))
        return try endCities.map({endCity in try getMinimumDistance(startCity, cities: endCities, endCity: endCity, paths: cityPaths)}).reduce(Int.max, combine: {min($0, $1)})
    }).reduce(Int.max, combine: {min($0, $1)})
    
    let maxDistance = try cities.map({startCity -> Int in
        let endCities = cities.filter(({city in city != startCity}))
        return try endCities.map({endCity in try getMaximumDistance(startCity, cities: endCities, endCity: endCity, paths: cityPaths)}).reduce(0, combine: {max($0, $1)})
    }).reduce(0, combine: {max($0, $1)})

    print("Part 1: \(minDistance)")
    print("Part 2: \(maxDistance)")
}
catch let caught as NSError {
    print("error: " + caught.description)
}

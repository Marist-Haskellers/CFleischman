# Journal Entry for "The Amazing Human Cannonball"

---

## Problem Description

The problem involves simulating a cannonball's trajectory. Given the input parameters: initial `velocity`, launch `angle` (in degrees), horizontal `distance`, and two height constraints (`heightLow` and `heightUp`), determine if the cannonball successfully passes through the lower and upper height constraints.

---

## Approach and Solution

### Understanding the Physics

The trajectory of the cannonball follows the equations provided in the problem description. To derive the formula for height `y` at a given horizontal `distance`, we combined two equations of projectile motion:

- The **horizontal distance equation**:
  
  ```plaintext
  distance = velocity * cos(theta) * time
  ```
  
  where `time` is the time of flight, `velocity` is the initial speed of the cannonball, and `theta` (converted from `angle`) is the launch angle in radians.

- The **vertical height equation**:
  
  ```plaintext
  height = velocity * sin(theta) * time - 0.5 * gravity * time²
  ```
  
  where `gravity` is the gravitational acceleration (9.81 m/s²).

We solved for `time` using the **horizontal distance equation**:

```plaintext
time = distance / (velocity * cos(theta))
```

Substituting `time` into the **vertical height equation** gives:

```plaintext
height = distance * tan(theta) - (gravity * distance²) / (2 * velocity² * cos(theta)²)
```

In the code, this final equation calculates `height` (denoted as `y`) at the given `distance`.

### Input Parsing

The input consists of multiple test cases, each providing five parameters:

- `velocity`: Initial velocity of the cannonball.
- `angle`: Launch angle in degrees.
- `distance`: Horizontal distance the cannonball travels.
- `heightLow`: Minimum height of the window.
- `heightUp`: Maximum height of the window.

The input is parsed using:

```haskell
parseInput :: Input -> [[Double]]
parseInput input = map (map read . words) (tail (lines input))
```

### Logic Implementation

The logic calculates the trajectory height `y` at the given `distance`:

```haskell
y = distance * tan(theta) - (gravity * distance²) / (2 * velocity² * cos(theta)²)

```

Then it checks if `y` lies strictly within the safe window:

```haskell
if y > heightLow + 1 && y < heightUp - 1 then "Safe" else "Not Safe"
```

### Output

For each test case, the program prints:

- `"Safe"` if the trajectory height meets the conditions.
- `"Not Safe"` otherwise.

---

## Challenges and Debugging

### Combining the Formulas

Replacing `time` from the horizontal equation into the vertical equation required proper use of different trigonometric properties. Using descriptive variable names (`velocity`, `angle`, `distance`, etc.) helped track calculations.

### Parsing Input

Building out the logic to parse the problems input into a readable form for the equation was difficult. The `parseInput` function breaks down what needed to be performed at each step to transform the data into useable information.

### Testing

- Fulfilled the Kattis test cases with varying `velocity`s, `angle`s, and `distance`s to validate calculations.

---

## Code

```haskell
type Input = String

type Output = String

main :: IO ()
main = interact $ showResult . doTheWork . parseInput

parseInput :: Input -> [[Double]]
parseInput input = map (map read . words) (tail (lines input))

doTheWork :: [[Double]] -> [String]
doTheWork =
  map
    ( \[velocity, angle, distance, heightLow, heightUp] ->
        let gravity = 9.81
            theta = angle * pi / 180
            time = distance / (velocity * cos theta)
            position = velocity * time * sin theta - 0.5 * gravity * time ^ 2
         in if position > heightLow + 1 && position < heightUp - 1
              then "Safe"
              else "Not Safe"
    )

showResult :: [String] -> Output
showResult = unlines
```

---

## Screen Shot

![Kattis problem completion](./AHC.png)

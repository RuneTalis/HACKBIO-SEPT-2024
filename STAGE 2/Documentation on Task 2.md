# Laboratory Reagent Preparation Calculator – Documentation of Formulas and Logic

This document provides a detailed explanation of the mathematical formulas used in each calculator module of the Laboratory Reagent Preparation Calculator. It also outlines the logic for how each calculation was implemented in R Shiny.

---

## 1. Serial Dilution Calculator

### Mathematical Formula:
The formula for serial dilution is derived from the equation:

V1 = (C2 * V2) / C1
Where:
- V1 = Volume of the stock solution needed (mL)
- C1 = Initial concentration (M)
- C2 = Desired final concentration (M)
- V2 = Total volume of the final solution (mL)



### Explanation:
This formula determines the amount of stock solution (V1) required to achieve a desired concentration (C2) in the final solution. The dilution factor is implicitly accounted for by the ratio of the initial and final concentrations. The stock solution is diluted by mixing it with a solvent to reach the final volume (V2) and concentration (C2).

### Implementation in R Shiny:
In R Shiny, the logic for this calculation is triggered when the user selects the **Serial Dilution** option and clicks the calculate button. The calculation multiplies the final concentration by the total volume, then divides by the initial concentration to compute the stock volume required.

```r
observeEvent(input$calculate, {
    if (input$calculation == "Serial Dilution") {
        stock_vol <- input$total_volume * (input$final_concentration / input$initial_concentration)
        output$result <- renderText({
            paste("To achieve a final concentration of", input$final_concentration, 
                  "M, you need to transfer", round(stock_vol, 2), 
                  "mL of the stock solution.")
        })
    }
})
```

---

## 2. Stock Solution Dilution Calculator

### Mathematical Formula:
The formula used for stock solution dilution is:

C1 * V1 = C2 * V2
Rearranging for V1:
V1 = (C2 * V2) / C1


Where:
- V1 = Volume of the stock solution needed (mL)
- V2 = Concentration of the stock solution (M)
- V2 = Desired concentration in the final solution (M)
- V2 = Final volume of the solution (mL)

### Explanation:
This formula ensures that the total moles of solute before dilution (in the stock solution) are equal to the total moles after dilution. The calculator determines the volume of the concentrated stock solution that must be diluted to prepare a solution with the desired concentration.

### Implementation in R Shiny:
In R Shiny, this logic is similar to that of the serial dilution. The calculation uses the input values for stock concentration, desired concentration, and final volume to calculate the amount of stock solution required.

```r
observeEvent(input$calculate, {
    if (input$calculation == "Stock Solution Dilution") {
        stock_vol <- input$final_volume * (input$desired_concentration / input$stock_concentration)
        output$result <- renderText({
            paste("To achieve a final concentration of", input$desired_concentration, 
                  "M, you need to transfer", round(stock_vol, 2), 
                  "mL of the stock solution.")
        })
    }
})
```

---

## 3. Molarity Calculation

### Mathematical Formula:
The formula for molarity calculation is:

Mass (g) = Molecular Weight (g/mol) * Desired Molarity (M) * Solution Volume (L)

Where:
- **Mass of Solute** = Mass of the solute required (g)
- **Molecular Weight** = Molecular weight of the solute (g/mol)
- **Desired Molarity** = Target molarity of the solution (M)
- **Solution Volume** = Volume of the solution being prepared (L)

### Explanation:
This formula calculates how much solute is needed to prepare a solution of a specified molarity and volume. The number of moles of solute is determined by multiplying the desired molarity by the solution volume. The mass of the solute is then calculated by multiplying this by the molecular weight of the compound.

### Implementation in R Shiny:
The app calculates the mass of solute based on user input, including the molecular weight, desired molarity, and solution volume. The result is displayed in grams.

```r
observeEvent(input$calculate, {
    if (input$calculation == "Molarity Calculation") {
        mass_solute <- input$molecular_weight * input$desired_molarity * input$solution_volume
        output$result <- renderText({
            paste("To make", input$solution_volume, "L of a", input$desired_molarity, 
                  "M solution, you need to weigh out", round(mass_solute, 2), "g of solute.")
        })
    }
})
```

---

## 4. Buffer Preparation Calculation

### Mathematical Formula:
The formula used for buffer preparation is a simplified version of buffer calculations, often based on the Henderson-Hasselbalch equation:

For simplicity, the buffer preparation calculation uses the following formula:
Vacid = Vbuffer * (pH / [acid] + [base])

Where:
- Vacid = Volume of the acid solution needed (L)
- Vbuffer = Total volume of the buffer solution (L)
- [acid] = Concentration of the acid (M)
- [base] = Concentration of the base (M)
- pH = Desired pH of the buffer solution

### Explanation:
This simplified formula calculates the amount of acid required to achieve a buffer solution with the desired pH. The actual buffer preparation may involve more complex calculations, especially for weak acids and bases, but this provides a foundational understanding for buffer systems.

### Implementation in R Shiny:
The R Shiny logic calculates the volume of acid required based on the total buffer volume, acid and base concentrations, and the desired pH.

```r
observeEvent(input$calculate, {
    if (input$calculation == "Buffer Preparation") {
        acid_vol <- input$buffer_volume * (input$pH / (input$acid_concentration + input$base_concentration))
        output$result <- renderText({
            paste("For a buffer of pH", input$pH, "with a volume of", input$buffer_volume, 
                  "L, you need to add", round(acid_vol, 2), "L of acid solution.")
        })
    }
})
```

---

This documentation outlines the core mathematical principles and corresponding R Shiny implementation for each calculator module. These formulas drive the app's functionality, ensuring accurate reagent preparation based on user inputs.

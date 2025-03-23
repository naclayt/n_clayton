# Identifying MLS Quality Players to Target

The intent of this project was to analyze event level data from an MLS-equivalent league to identify 3 high-quality attacking players worthy of being considered as transfer candidates.

# Methodology
1. **Initial Data Processing**:
- Collected the initial dataset.
- Calculated league averages for key variables related to striker success.
- Filtered down to potential strikers based on position.
2. **Key Variable Selection**:
- Identified relevant variables that best indicate successful strikers.
- Calculated the following for each player:
  -   Shot Quality
  -   Shot Execution
  -   Shot Under Pressure
3. **Filtering Process**:
- Considered only players with a certain shot volume to adjust for sample size.
- Compared individual player stats to league averages.
- Filtered down to players who were above league average in all three key metrics.
4. **Effectiveness Score Calculation**:
- Created an effectiveness score by combining shot quality, execution, and under-pressure performance.
- Identified the top ten players with the highest effectiveness scores.
5. **Advanced Player Evaluation**:
- Conducted a heat map analysis to highlight strengths across shot quality, execution, and under-pressure performance.
- Used shot execution as the next filtering step, as it is the most translatable skill across different leagues.
- Identified four players who met all prior criteria and were above the trendline for shot execution.
6. **Final Selection**:
- Evaluated the final candidates: 4269, 44263, 139438, and 42954.
- Considered holistic shot quality, execution, and consistency under pressure.
- Used a tie-breaker approach to finalize selections, prioritizing consistency over isolated strengths.

***Final Three Players Selected***:
 
  ✅ 4269
  ✅ 44263
  ✅ 139438

**Data Variables Used**
The following variables were instrumental in creating the effectiveness score and final selections:

- under_pressure
- avevelocity
- shot.statsbomb_xg
- shot.shot_execution_xg
- shot.shot_execution_xg_uplift
- shot.outcome.name

**Conclusion**

This analysis successfully identified the top three strikers by systematically filtering, ranking, and analyzing key shot metrics. By focusing on shot execution as the most translatable skill, the final selection represents the most well-rounded and effective players.

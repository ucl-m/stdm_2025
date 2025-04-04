import pandas as pd

def return_data():
    return("Hello, world!")


# Function to add missing hours with zero collisions
def get_missing_hours(dataset: pd.DataFrame):
    current_id = 0
    last_id = 0
    expected_hour = 0
    current_hour = 0
    max_hour = 23
    missing_entries = {}


    dataset.hour = dataset.hour.astype(int)
    dataset = dataset.sort_values(by=['poly_id', 'hour']).reset_index(drop=True)
    
    for index, row in dataset.iterrows():
        current_id = row['poly_id']
        current_hour = row['hour']

        # If new poly_id
        if current_id != last_id:
            if (last_id != 0):
                # Check and add missing hours
                while (expected_hour < max_hour):
                    expected_hour += 1
                    print("--- Adding hour", expected_hour)
                    # Create missing row in format [poly_id, hour, count, mean_count]
                    missing_entries[f"{last_id}-{expected_hour}"] = [last_id, expected_hour, 0, 0.0]

            last_id = current_id
            expected_hour = 0
            print('Polygon:', current_id)
        else:
            expected_hour += 1
        
        # Check hour exists, if not add to list of missing
        while ((expected_hour < current_hour) and (expected_hour <= max_hour)):
            print("-- Adding hour", expected_hour)
            # Create missing row in format [poly_id, hour, count, mean_count]
            missing_entries[f"{current_id}-{expected_hour}"] = [current_id, expected_hour, 0, 0.0]

            expected_hour += 1
    
        missing_df = pd.DataFrame.from_dict(missing_entries, orient='index', columns=list(dataset))

    return missing_df
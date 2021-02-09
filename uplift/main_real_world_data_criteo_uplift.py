import os
from uplift_real_world import uplift_real_world
import time as t


def main():
    start = t.time()

    data_name = 'criteo_uplift'
    col_number = 14
    treatment = 'treatment'
    outcome = 'visit'

    try:
        os.mkdir('../' + '/real_world_data/data_' + str(data_name) + '/' +
                 'model_vs_contingency_table_' + str(data_name) + '_uplift')
    except OSError as e:
        pass

    for time in range(1, 11):
        for fold in range(1, 3):
            input_csv_name = '../real_world_data/data_' \
                             + str(data_name) + '/model_vs_contingency_table_' \
                             + str(data_name) + '/cross_validation_time_' \
                             + str(time) + '_fold_' + str(fold) + '_for_model.csv'
            ground_truth_csv_name = '../' + '/real_world_data/data_' \
                                    + str(data_name) + '/model_vs_contingency_table_' \
                                    + str(data_name) + '/cross_validation_time_' \
                                    + str(time) + '_fold_' + str(fold) + '_as_ground_truth.csv'
            image_name = '../' + '/real_world_data/data_' \
                         + str(data_name) + '/model_vs_contingency_table_' \
                         + str(data_name) + '_uplift/cross_validation_time_' \
                         + str(time) + '_fold_' + str(fold) + '.png'
            save_csv_name = '../' + '/real_world_data/data_' \
                            + str(data_name) + '/' + 'model_vs_contingency_table_' \
                            + str(data_name) + '_uplift/model_vs_contingency_table_cv_' \
                            + str(time) + '_fold_' + str(fold) + '.csv'
            save_ground_truth_name = '../' + '/real_world_data/data_' \
                                     + str(data_name) + '/model_vs_contingency_table_' \
                                     + str(data_name) + '_uplift/cross_validation_time_' \
                                     + str(time) + '_fold_' + str(fold) + '_as_ground_truth.csv'

            uplift_real_world(input_csv_name, ground_truth_csv_name, col_number, treatment, outcome, image_name,
                              save_csv_name, save_ground_truth_name)

    end = t.time()
    print(end - start)


if __name__ == '__main__':
    main()

import pandas as pd
from uplift_tree_self_training import uplift_tree_self_training
from uplift_tree_cross_validation import uplift_tree_cross_validation


def main():
    # folder_names = ['data_20_4_4', 'data_40_4_4', 'data_60_4_4', 'data_80_4_4', 'data_100_4_4']
    folder_names = [20, 40, 60, 80, 100]
    max_depth = 9

    # self-training
    for folder_name in folder_names:
        print(folder_name)
        for i in range(1, 11):
            batch_number = i
            input_csv_name = ('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                              '/causal_trees_self_training/causal_tree_self_training_batch_' + str(i) + '.csv')
            image_name = ('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                          '/uplift_tree_self_training/uplift_tree_self_training_batch_' + str(i) + '.png')

            predictions = uplift_tree_self_training(folder_name, batch_number, input_csv_name, image_name, max_depth)
            predictions = pd.DataFrame(predictions)
            predictions.to_csv('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                               '/uplift_tree_self_training/uplift_tree_self_training_batch_' + str(i) + '.csv')

    # cross-validation
    for folder_name in folder_names:
        print(folder_name)
        for i in range(1, 11):
            batch_number = i
            for j in range(1, 11):
                fold_number = j
                train_csv_name = ('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                                  '/causal_trees_cross_validation/causal_tree_cross_validation_batch_' + str(i) +
                                  '_fold_' + str(j) + '_train.csv')
                test_csv_name = ('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                                 '/causal_trees_cross_validation/causal_tree_cross_validation_batch_' + str(i) +
                                 '_fold_' + str(j) + '_test.csv')
                image_name = ('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                              '/uplift_tree_cross_validation/uplift_tree_cross_validation_batch_' + str(i) +
                              '_fold_' + str(j) + '.png')

                predictions = uplift_tree_cross_validation(folder_name, batch_number, train_csv_name, test_csv_name,
                                                           image_name, max_depth)
                predictions = pd.DataFrame(predictions)
                predictions.to_csv('../synthetic_data/' + 'data_' + str(folder_name) + '_4_4' +
                                   '/uplift_tree_cross_validation/uplift_tree_cross_validation_batch_' + str(i) +
                                   '_fold_' + str(j) + '.csv')


if __name__ == '__main__':
    main()

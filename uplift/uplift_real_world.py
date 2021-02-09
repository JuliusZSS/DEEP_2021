# Input: real world data
# Output: uplift result

def uplift_real_world(input_csv_name, ground_truth_csv_name, col_number, treatment, outcome, image_name,
                      save_csv_name, save_ground_truth_name):
    import pandas as pd
    import numpy as np
    from causalml.inference.tree import UpliftTreeClassifier
    from IPython.display import Image
    from causalml.inference.tree import uplift_tree_plot

    # parameters
    max_depth = 9

    # read in data
    data = pd.read_csv(input_csv_name)

    # define treatment and outcome
    cols = list(range(1, col_number + 1))
    idx_treatment = data.columns.get_loc(treatment)
    idx_outcome = data.columns.get_loc(outcome)
    cols.remove(idx_treatment)
    cols.remove(idx_outcome)
    features_name = list(data.columns[i] for i in cols)
    features_name_pattern = features_name[:]
    features_name_pattern.append('causal_effect')

    # set up uplift model
    uplift_model = UpliftTreeClassifier(max_depth=max_depth, min_samples_leaf=200, min_samples_treatment=50,
                                        n_reg=100, evaluationFunction='ED', control_name='0')

    # train model
    uplift_model.fit(data.iloc[:, cols].values,
                     treatment=(data[treatment].astype(str)).values,
                     y=data[outcome].values)

    # visualise model
    graph = uplift_tree_plot(uplift_model.fitted_uplift_tree, features_name)
    uplift_model_structure = Image(graph.create_png())
    uplift_model_png = image_name
    with open(uplift_model_png, 'wb+') as o:
        o.write(uplift_model_structure.data)

    # get prediction using train dataset
    predictions = uplift_model.predict(data.iloc[:, cols].values)

    # data + causal effect = new dataframe
    new_data = data.copy()
    new_data = new_data.drop(new_data.columns[0], axis=1)
    new_data = new_data.drop(['leaf_index'], axis=1)
    new_data = new_data.drop([treatment, outcome], axis=1)
    new_data['causal_effect'] = predictions[1]

    # merge new dataframe to find patterns
    patterns_list = list(set(new_data['causal_effect']))

    # find patterns of tree nodes
    root_nodes = pd.DataFrame(index=range(len(set(predictions[1]))), columns=features_name_pattern)

    # start loop here
    for pattern_index in range(len(patterns_list)):

        pattern_causal_effect = patterns_list[pattern_index]

        pattern_rows = new_data[new_data['causal_effect'] == pattern_causal_effect].copy()

        # find if column is unique
        for col in pattern_rows:
            if len(set(pattern_rows[col])) != 1:
                pattern_rows[col] = np.nan

        root_nodes.iloc[pattern_index, :] = pattern_rows.iloc[0, :]

    # initialise contingency table
    root_nodes[['n11', 'n12', 'n21', 'n22', 'pi_1', 'pi_2', 'phi', 'bias', 'N']] = 0

    # read in ground truth data
    ground_truth = pd.read_csv(ground_truth_csv_name)
    ground_truth_copy = ground_truth.copy()
    ground_truth = ground_truth.drop(ground_truth.columns[0], axis=1)
    ground_truth = ground_truth.drop(['CATE'], axis=1)

    # get prediction using test dataset
    predictions_using_ground_truth = uplift_model.predict(ground_truth_copy.iloc[:, cols].values)
    ground_truth_copy['CATE'] = predictions_using_ground_truth[1]
    ground_truth_copy.to_csv(save_ground_truth_name, index=False)

    for causal_effect in patterns_list:
        subset = ground_truth_copy.loc[ground_truth_copy['CATE'] == causal_effect]
        subset_n11 = subset.loc[(subset[treatment] == 1) & (subset[outcome] == 1)].shape[0]
        subset_n12 = subset.loc[(subset[treatment] == 1) & (subset[outcome] == 0)].shape[0]
        subset_n21 = subset.loc[(subset[treatment] == 0) & (subset[outcome] == 1)].shape[0]
        subset_n22 = subset.loc[(subset[treatment] == 0) & (subset[outcome] == 0)].shape[0]

        root_nodes.loc[root_nodes['causal_effect'] == causal_effect, 'n11'] = subset_n11
        root_nodes.loc[root_nodes['causal_effect'] == causal_effect, 'n12'] = subset_n12
        root_nodes.loc[root_nodes['causal_effect'] == causal_effect, 'n21'] = subset_n21
        root_nodes.loc[root_nodes['causal_effect'] == causal_effect, 'n22'] = subset_n22
        root_nodes.loc[root_nodes['causal_effect'] == causal_effect, 'N'] = subset_n11 + subset_n12 + subset_n21 + subset_n22

    # calculate pi_1 pi_2 phi and bias
    root_nodes['pi_1'] = root_nodes['n11'] / (root_nodes['n11'] + root_nodes['n12'])
    root_nodes['pi_2'] = root_nodes['n21'] / (root_nodes['n21'] + root_nodes['n22'])
    root_nodes['phi'] = root_nodes['pi_1'] - root_nodes['pi_2']
    root_nodes['bias'] = abs(root_nodes['phi'] - root_nodes['causal_effect'])

    root_nodes.to_csv(save_csv_name)

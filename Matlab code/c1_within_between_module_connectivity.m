%% Unifying modular structure + intra-mod connectivity
% Tried both sum and average

cons = {'fear','anger','sad','happy'} 

for c = 1:length(cons)

    load(sprintf('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/bs_network_%s_759s.mat',cons{c}));
    load(sprintf('community_2018/Community_%s_All.mat',cons{c}))

    N_Sub = size(bs_network,3);
    roi = length(roi_names);
    n_mod = length(unique(Partition));

    inter_mod_order = [];
    for i = 2:n_mod
        for j = 1:i - 1
            inter_mod_order = [inter_mod_order; i, j];
        end
    end
    inter_mod_order = sort(inter_mod_order, 2, 'ascend');

    all_intra_sum = zeros(N_Sub, n_mod);
    all_inter_sum = zeros(N_Sub, size(inter_mod_order, 1));
    all_intra_avg = zeros(N_Sub, n_mod);
    all_inter_avg = zeros(N_Sub, size(inter_mod_order, 1));

    
    for sub = 1:N_Sub
        disp(sub)
        Matrix = bs_network(:,:,sub);
        Matrix(1:roi + 1:end) = 0;

        MAT = Matrix; 
        intra_sum = zeros(n_mod, 1);
        intra_avg = zeros(n_mod, 1);
        for i = 1:n_mod
              tmp_mat = MAT(Partition == i, Partition == i);
              intra_sum(i) = (nansum(nansum(tmp_mat)) / 2);
              intra_avg(i) = nansum(nansum(tmp_mat)) / (size(tmp_mat,1)*size(tmp_mat,2)-size(tmp_mat,1));
        end
        inter_sum = zeros(size(inter_mod_order, 1), 1);
        inter_avg = zeros(size(inter_mod_order, 1), 1);
        for i = 1:length(inter_sum)
             tmp_mat = MAT(Partition == inter_mod_order(i, 1), Partition == inter_mod_order(i, 2));
             inter_sum(i) = nansum(nansum(tmp_mat));
             inter_avg(i) = nansum(nansum(tmp_mat))/(size(tmp_mat,1)*size(tmp_mat,2));
        end

        all_intra_sum(sub, :) = intra_sum;
        all_inter_sum(sub, :) = inter_sum;
        all_intra_avg(sub, :) = intra_avg;
        all_inter_avg(sub, :) = inter_avg;
    end

    inter_mod_order = inter_mod_order;
    fname = sprintf('within_between_2018/within_between_mc_%s_759s_weighted.mat',cons{c});
    save(fname, 'all_intra_sum', 'all_inter_sum','all_intra_avg', 'all_inter_avg', 'inter_mod_order');
end



clear; clc;
%---Print for R analysis---
cons_name = {'fear','anger','sad','happy'}; 
GM = {'M1','M2','M3','M12','M13','M23'};
GM_num = length(GM);

subjects = '/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/subjs_PNC_759.txt'; 
subjects = ReadList(subjects);

dvs = GM;

for i=1:length(cons_name) % for each condition
      load(sprintf('within_between_2018/within_between_mc_%s_759s_weighted.mat',cons_name{i}))
      
      for type=1:2 
          if type == 1
              filename = sprintf('within_between_2018/within_between_mc_%s_weighted_sum.txt', cons_name{i});
              all_intra = all_intra_sum;
              all_inter = all_inter_sum;
          else
              filename = sprintf('within_between_2018/within_between_mc_%s_weighted_avg.txt', cons_name{i});
              all_intra = all_intra_avg;
              all_inter = all_inter_avg;
          end
          fid = fopen(filename ,'w');

          fprintf(fid, 'Subject\t');

          % print header info
          for c=1:length(dvs) 
               fprintf(fid, '%s\t', dvs{c});
          end
          fprintf(fid, '\n');

          for ss = 1:length(subjects)
                fprintf(fid, '%s\t', subjects{ss});

                v = [all_intra(ss,:) all_inter(ss,:)];
                for cc = 1:length(dvs)
                    fprintf(fid, '%.4f\t', v(cc));
                end
                fprintf(fid, '\n');
          end
          fclose(fid);
      end
end





% %% Unifying modular structure + intra-mod connectivity (post hoc)
% load('Intra_inter_control.mat');
% load('Intra_inter_stress.mat');
% 
% p_intra = [];
% for i = 1:5
%     [~, p] = ttest(all_intra_control(:, i), all_intra_stress(:, i));
%     p_intra = [p_intra; p];
% end
% p_inter = [];
% for i = 1:10
%     [~, p] = ttest(all_inter_control(:, i), all_inter_stress(:, i));
%     p_inter = [p_inter; p];
% end

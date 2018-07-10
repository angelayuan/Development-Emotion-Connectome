%% Community Detection
% cons = {'fear','anger','sad','happy'};
% agebin = {'All','Adults','Adolescents','Children'};
% for c=1:length(cons)
%     fname = sprintf('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/bs_network_%s_759s.mat',cons{c})
%     load(fname); % load connectivity matrix (ROI*ROI*SUBJ)
%     
%     N_Sub = size(bs_network,3);
%     roi = size(bs_network,1);
%     
%     % Modularity of Group representative network
%     T = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/SubInfo_2018.txt','Delimiter','\t');
%     T = table2cell(T(:,10)); % agebin
%     
%     for i=1:length(agebin)
%         if(strcmp(agebin{i},'All')) %use all subjects
%             agg_graph = bs_network;
%         else % use adults or adolescents or children
%             ind = find(strcmp(T,agebin{i}));
%             agg_graph = bs_network(:,:,ind); 
%         end
%         % group average
%         agg_graph = nanmean(agg_graph,3);
%         agg_graph(1:roi + 1:end) = 0;
%         
%         % calculation
%         ITER = 100;
%         all_Ci = zeros(ITER, roi);
%         all_Q = zeros(ITER, 1);
%         h = waitbar(0, 'Modularity Analysis');
%         for iter = 1:ITER
%             waitbar(iter / ITER, h);
%             [Ci, Q] = modularity_louvain_und_sign(agg_graph);
%             all_Ci(iter, :) = Ci;
%             all_Q(iter) = Q;
%         end
%         close(h);
% 
%         [Q_max, max_ind] = max(all_Q);
%         Partition = all_Ci(max_ind, :);
%         
%         outputfile = sprintf('community_2018/Community_%s_%s.mat',cons{c},agebin{i});
%         save(outputfile, 'Partition', 'Q_max','agg_graph');
%      
%     end   
% end



%Q calculation for each individual
%% Modularity of individual subjects
cons = {'fear','anger','sad','happy'};

for c=1:length(cons)
    h = waitbar(0, 'Modularity Analysis');
    fname = sprintf('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/bs_network_%s_759s.mat',cons{c})
    load(fname); % load connectivity matrix (ROI*ROI*SUBJ)
 
    N_Sub = size(bs_network,3);
    roi = size(bs_network,1);
    
    all_Qs = zeros(N_Sub,1);
    all_mod = zeros(N_Sub,roi);
    
    %parpool(2);
    for sub = 1:N_Sub
        waitbar(sub / N_Sub, h);
        Mat = bs_network(:,:,sub);
        Mat(1:roi + 1:end) = 0;
        Mat(find(isnan(Mat))) = 0;
        
        % calculation
        ITER = 100;
        all_Ci = zeros(ITER, roi);
        all_Q = zeros(ITER, 1);
        for iter = 1:ITER
            [Ci, Q] = modularity_louvain_und_sign(Mat);
            all_Ci(iter, :) = Ci;
            all_Q(iter) = Q;
        end
        [Q_max, max_ind] = max(all_Q);
        Partition = all_Ci(max_ind, :);
        
        all_Qs(sub) = Q_max;
        all_mod(sub, :) = Partition;
        
    end
    close(h);
    
    
    f = sprintf('Q_%s_759s.mat',cons{c});
    save(f, 'all_Qs','all_mod');
end
%delete(gcp) %parpool close;


cons = {'fear','anger','sad','happy'};
T = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/SubInfo_2018.txt','Delimiter','\t');
age = table2array(T(:,2)); % age
[ages,ind] = sort(age);

c=1;
f = sprintf('Q_%s_759s.mat',cons{c});
load(f);

num_mod = max(all_mod,[],2);

all_Qs_sorted = all_Qs(ind);
num_mod_sorted = num_mod(ind);


% all_Q = zeros(N_Sub);
% all_mod = zeros(N_Sub,roi);
% 
% parpool(2);
% ITER = 100;
% for sub = 1:N_Sub
%     waitbar(sub / N_Sub, h);
%     disp(sub);
%     Matrix = fear(:,:,sub);
%     Matrix(1:roi + 1:end) = 0;
%     Matrix = (Matrix + Matrix') / 2;
%     
%     parfor n = 1:N_Spar
%         Mat_spar = threshold_proportional(Matrix,Spar(n));
%         all_Ci_tmp = zeros(ITER, roi);
%         all_Q_tmp = zeros(ITER, 1);
%         for iter = 1:ITER
%             [Ci, Q] = modularity_louvain_und_sign(Mat_spar);
%             all_Ci_tmp(iter, :) = Ci;
%             all_Q_tmp(iter) = Q;
%         end
%         [Q_max, max_ind] = max(all_Q_tmp);
%         Ci_max = all_Ci_tmp(max_ind, :);   
%         all_Q(sub, n) = Q_max;
%         all_mod(sub, n, :) = Ci_max;
%     end
% end
% close(h);
% 
% all_Q_auc = zeros(N_Sub, 1);
% for sub = 1:N_Sub
%     all_Q_auc(sub) = trapz(Spar, all_Q(sub, :));
% end
% 
% save('Q_fear_759s.mat', 'all_Q','all_mod','all_Q_auc');
% delete(gcp) %parpool close;

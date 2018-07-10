%% Calculate degree and identify hubs
roi = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/roiname_list.txt','Delimiter','\t','ReadVariableNames',false);
roi_name = table2cell(roi);
roi_num = length(roi_name);

% Spar = 0.1:0.05:0.4;
% N_Spar = length(Spar);
% 
% cons = {'fear','anger','sad','happy'};
% 
% for c=1:length(cons)
%     f = sprintf('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/bs_network_%s_759s.mat',cons{c});
%     load(f)
%     N_Sub = size(bs_network,3);
%     
%     % load('community/Community_759_anger.mat'); % useful for PC
%     Node_Deg = zeros(roi_num, N_Sub, N_Spar); % degree
%     % Node_PC = zeros(roi_num, N_Sub, N_Spar); % participation coefficient
% 
%     h = waitbar(0, 'Graph Analysis');
%     %parpool(2);
%     for sub = 1:N_Sub
%         %fprintf('sub%d\n',sub)
%         waitbar(sub / N_Sub, h);
%         Matrix = abs(bs_network(:,:,sub)); 
%         % use abs matrix because degree calculation does not change 
%         % with valence but threshold_proportional may be influence by valence
%         Matrix(1:roi_num + 1:end) = 0;
%         Matrix(isnan(Matrix)) = 0;% for some participants, there are NAN values
% 
%         if any(any(Matrix))
%             for n = 1:N_Spar
%                 Mat_spar = threshold_proportional(Matrix,Spar(n));
%                 Node_Deg(:,sub,n) = degrees_und(Mat_spar);
%     %           Node_PC(:,sub,n) = participation_coef(Mat_spar, Partition);
%             end
%         end
%     end
%     %delete(gcp)
%     close(h)
%     
%     % AUC
%     for s = 1:N_Sub
%         for r = 1:roi_num
%             Node_Deg_auc(r, s) = trapz(Spar, Node_Deg(r, s, :)); % 50*759
%         end
%     end
% 
%     save(sprintf('graph theory/degree/Degree_%s_759s.mat',cons{c}),'Node_Deg','Node_Deg_auc');   
% end

%% Hub identification
% find global hub (Deg > mean + SD)
T = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/SubInfo_2018.txt','Delimiter','\t');
age = table2array(T(:,2)); % age
agebin = table2array(T(:,10)); % agebin

cons = {'fear','anger','sad','happy'};

for c = 1:length(cons) 
    fname = sprintf('degree/Degree_%s_759s.mat',cons{c})
    load(fname)
    
    Deg_hub_name = [];
    Deg_hub = [];
    Deg_hub_mask = [];
    % age year
    for i=8:22 
        dat = Node_Deg_auc(:,find(age>=i & age<i+1)); % ROI*SUBJ
        %Deg_avg = nanmean(dat,2);
        % normalize
        Deg_avg = roi_num * nansum(dat,2)/nansum(nansum(dat,2));
        Deg_mask = (Deg_avg >= mean(Deg_avg) + std(Deg_avg));
        Deg_hub_name{i-7} = roi_name(Deg_mask);
        Deg_hub{i-7} = Deg_avg(Deg_mask);
        Deg_hub_mask{i-7} = Deg_mask;
    end
    %fname = sprintf('degree/hubs_NrzDegAuc_%s_ageyear.mat',cons{c});
    %save(fname,'Deg_hub_name','Deg_hub','Deg_hub_mask');
    
    % organize for plot
    hub_deg = zeros(roi_num,15); % ROI*Age
    for i=1:15
        hub_deg(find(Deg_hub_mask{i}==1),i) = Deg_hub{i};
    end
   % fname = sprintf('degree/%s_Nrzhubs_age_year.mat',cons{c});
   % save(fname,'hub_deg')
    
    %plot
    figure;
    imagesc(hub_deg)
    xticklabels = 8:22;
    xticks = linspace(1, size(hub_deg,2), numel(xticklabels));
    set(gca, 'XTick', xticks, 'XTickLabel', xticklabels);

    yticklabels = roi_name;
    yticks = linspace(1, size(hub_deg,1), numel(yticklabels));
    set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
    
    fname = sprintf('degree/%s_Nrzhubs_ageyear.fig',cons{c});
    savefig(fname);
    
    %---agebin---
    bin = {'Children','Adolescents','Adults'};
    Deg_hub_name = [];
    Deg_hub = [];
    Deg_hub_mask = [];
    for i=1:3 
        dat = Node_Deg_auc(:,find(strcmp(agebin,bin{i}))); %Node_Deg_auc(:,find(age==i));
        %Deg_avg = nanmean(dat,2);
        % normalize
        Deg_avg = roi_num * nansum(dat,2)/nansum(nansum(dat,2));
        Deg_mask = (Deg_avg >= mean(Deg_avg) + std(Deg_avg));
        Deg_hub_name{i} = roi_name(Deg_mask);
        Deg_hub{i} = Deg_avg(Deg_mask);
        Deg_hub_mask{i} = Deg_mask;
    end
   % fname = sprintf('degree/hubs_NrzDegAuc_%s_agebin.mat',cons{c});
    %save(fname,'Deg_hub_name','Deg_hub','Deg_hub_mask')

    % organize hubs for each agebin
    hub_deg = zeros(roi_num,3); % ROI*Age
    for i=1:3
        hub_deg(find(Deg_hub_mask{i}==1),i) = Deg_hub{i};
    end
   % fname = sprintf('degree/%s_Nrzhubs_agebin.mat',cons{c});
   % save(fname,'hub_deg')

    figure;
    imagesc(hub_deg)
    xticklabels = {'Children','Adolescents','Adults'};
    xticks = linspace(1, size(hub_deg,2), numel(xticklabels));
    set(gca, 'XTick', xticks, 'XTickLabel', xticklabels);

    yticklabels = roi_name;
    yticks = linspace(1, size(hub_deg,1), numel(yticklabels));
    set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
    
    fname = sprintf('degree/%s_Nrzhubs_agebin.fig',cons{c});
    savefig(fname);
end




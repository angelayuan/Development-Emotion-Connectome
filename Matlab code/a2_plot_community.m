
cons = {'fear','anger','sad','happy'};
agebin = {'All','Adults','Adolescents','Children'};

for c=1:length(cons)
    for i=1:length(agebin)
        fname = sprintf('community_2018/Community_%s_%s.mat',cons{c},agebin{i});
        load(fname)
        
        if i==1
            matrix = Partition';
        else
            matrix = [matrix Partition'];
        end
    end
    
    % plot
    roi = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/roiname_list.txt','Delimiter','\t','ReadVariableNames',false);
    roi_name = table2cell(roi);
    roi_num = length(roi_name);
        
    imagesc(matrix)
    xticklabels = agebin;
    xticks = linspace(1, size(matrix,2), numel(xticklabels));
    set(gca, 'XTick', xticks,'XTickLabel', xticklabels);

    yticklabels = roi_name;
    yticks = linspace(1, length(roi_name), numel(yticklabels));
    set(gca, 'YTick', yticks, 'YTickLabel', yticklabels(:));
    
    fig = sprintf('community_2018/community_%s.fig',cons{c});
    savefig(fig);
end

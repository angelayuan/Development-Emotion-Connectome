
cons_name = {'fear','angry','threat'}; %{'neutral','fear','angry','sad','happy'};
roi_name = ReadList('roiname_list.txt');
roi_num = length(roi_name);

subjects = 'subjs_PNC_759.txt'; 
subjects = ReadList(subjects);

for n = 1:roi_num
   ROI_labels{n} = roi_name{n}; 
end

dvs = {};
properties = {'Deg','PC'};
for t = 1:length(properties)
    for i=1:roi_num
          cname = sprintf('%s_%s', roi_name{i},properties{t});
          dvs = [dvs cname];
    end
end

for i=1:length(cons_name) % for each condition
      filename = sprintf('nodal_properties_%s.txt', cons_name{i});
      fid      = fopen(filename ,'w');
      
      load(sprintf('GTA_%s_759s.mat',cons_name{i}));
      fprintf(fid, 'Subject\t');
      
      % print header info
      for c=1:length(dvs) 
           fprintf(fid, '%s\t', dvs{c});
      end
      fprintf(fid, '\n');
      
      for ss = 1:length(subjects)
            fprintf(fid, '%s\t', subjects{ss});
            
            v = [Node_Deg_auc(:,ss); Node_PC_auc(:,ss)];
            for cc = 1:length(dvs)
                fprintf(fid, '%.4f\t', v(cc));
            end
            fprintf(fid, '\n');
      end
      fclose(fid);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--skills: how to extract upper triangular and compact vector
% B = triu(ones(5,5),1);
% v = A(B==1);
% 
% A = B;
% A(B==1) = v;
%%%%%%%%%%%%%%

cons_name = {'neutral','fear','angry','sad','happy','threat','all','salient'};
%{'salient'}; %{'neutral','fear','angry','sad','happy'};
roi_name = ReadList('roiname_list.txt');
roi_num = length(roi_name);

subjects = 'subjs_PNC_759.txt'; 
subjects = ReadList(subjects);

for n = 1:roi_num
   ROI_labels{n} = roi_name{n}; 
end

conns = {};
for j=1:roi_num-1
    for i=j+1:roi_num
           cname = sprintf('%s_%s', roi_name{j},roi_name{i});
           conns = [conns cname];
    end
end

for i=1:length(cons_name) % for each condition
      filename = sprintf('bs_conns_%s.txt', cons_name{i});
      fid      = fopen(filename ,'w');
      
      load(sprintf('bs_network_%s_759s.mat',cons_name{i}));
      fprintf(fid, 'Subject\t');
      
      % print header info
      for c=1:length(conns) 
           fprintf(fid, '%s\t', conns{c});
      end
      fprintf(fid, '\n');
      
      for ss = 1:length(subjects)
            fprintf(fid, '%s\t', subjects{ss});
            
            mask = tril(ones(roi_num,roi_num),-1);
            mat = bs_network(:,:,ss);
            v = mat(mask==1);
            
            for cc = 1:length(conns)
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
package cz.bbn.cerberus.project.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.project.ProjectState;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.ui.component.tab.ProjectDetailTab;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.HashSet;

public class ProjectNewDialog extends AppDialog {

    private final AppInfiniteGrid<?> grid;
    private final ProjectComponentOperation projectComponentOperation;
    private final AppEnv appEnv;
    private final SubjectDto subjectDto;
    private final ContractDto contractDto;
    private final ListService listService;

    public ProjectNewDialog(AppInfiniteGrid<?> grid,
                            ProjectComponentOperation projectComponentOperation, AppEnv appEnv,
                            SubjectDto subjectDto, ContractDto contractDto, ListService listService) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.projectComponentOperation = projectComponentOperation;
        this.subjectDto = subjectDto;
        this.contractDto = contractDto;
        this.listService = listService;
        init();
    }

    void init() {
        setTitle(Transl.get("Add project"));

        ProjectDto dto = new ProjectDto();
        dto.setUserDto(SecurityUtils.getCurrentUserDto());
        dto.setSubject(subjectDto);
        dto.setContract(contractDto);
        dto.setProjectState(ProjectState.REALIZE);

        Button submit = VaadinComponents.getSubmitButton();

        ProjectDetailTab projectDetailTab =
                new ProjectDetailTab(dto, projectComponentOperation, HashSet::new, this, appEnv, true, false, listService);
        setContent(projectDetailTab);

        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            projectDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}

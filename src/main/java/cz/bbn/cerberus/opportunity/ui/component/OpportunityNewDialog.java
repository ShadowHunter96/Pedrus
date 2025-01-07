package cz.bbn.cerberus.opportunity.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.ui.component.tabs.OpportunityDetailTab;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.HashSet;

public class OpportunityNewDialog extends AppDialog {

    private final AppInfiniteGrid<OpportunitySimpleDto> grid;
    private final AppEnv appEnv;
    private final OpportunityComponentOperation opportunityComponentOperation;
    private final SubjectDto subjectDto;
    private final ListService listService;

    public OpportunityNewDialog(AppInfiniteGrid<OpportunitySimpleDto> grid, AppEnv appEnv,
                                OpportunityComponentOperation opportunityComponentOperation,
                                SubjectDto subjectDto, ListService listService) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.subjectDto = subjectDto;
        this.listService = listService;

        initComponent();
    }

    void initComponent() {
        setTitle(Transl.get("Add opportunity"));

        OpportunityDto dto = new OpportunityDto();
        dto.setProgress(0);
        dto.setSuccessChance(0);
        dto.setUser(SecurityUtils.getCurrentUserDto());
        SubjectDto defaultOwnCompany = listService.getSubjectDtoListByOwnCompany().stream()
                .filter(actualSubjectDto -> actualSubjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
        dto.setPrimarySupplier(defaultOwnCompany);
        if(subjectDto != null && !subjectDto.getOwnCompany()){
            dto.setSubject(subjectDto);
        }

        Button submit = VaadinComponents.getSubmitButton();

        OpportunityDetailTab opportunityDetailTab = new OpportunityDetailTab(dto, appEnv,
                opportunityComponentOperation, HashSet::new, this,
                true, false, null,
                listService);
        setContent(opportunityDetailTab);

        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            opportunityDetailTab.saveItem();
            if(grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }
}

package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.component.tab.SubjectDetailTab;
import cz.bbn.cerberus.translation.Transl;

import java.util.HashSet;

public class SubjectNewDialog extends AppDialog {

    private final AppInfiniteGrid<SubjectDto> grid;
    private final SubjectComponentOperation subjectComponentOperation;
    private final SubjectDto dto;
    private final AppEnv appEnv;
    private final ListService listService;

    public SubjectNewDialog(AppInfiniteGrid<SubjectDto> grid,
                            SubjectComponentOperation subjectComponentOperation, SubjectDto subjectDto,
                            AppEnv appEnv, ListService listService) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.subjectComponentOperation = subjectComponentOperation;
        this.dto = subjectDto;
        this.listService = listService;
        init();
    }

    private void init() {
        setTitle(getNewSubjectHeading(dto.getLocalSubject()));

        dto.setUserDto(SecurityUtils.getCurrentUserDto());

        Button submit = VaadinComponents.getSubmitButton();

        String ownCompany = subjectComponentOperation.getOwnNameCompany();
        SubjectDetailTab subjectDetailTab = new SubjectDetailTab(dto, subjectComponentOperation, appEnv,
                this, HashSet::new, true, false, listService.getSupplierTypeDtoList(),
                ownCompany != null && !ownCompany.isEmpty(), ownCompany);
        setContent(subjectDetailTab);

        submit.setDisableOnClick(true);
        submit.addClickListener(event -> {
            subjectDetailTab.saveItem();
            if (grid != null) {
                grid.loadData();
            }
            submit.setEnabled(true);
        });

        showWarning(true);

        addCloseButton();
        addButtons(submit);
    }

    private String getNewSubjectHeading(Boolean localSubject) {
        if (Boolean.TRUE.equals(localSubject)) {
            return Transl.get("Add CZ subject");
        }
        return Transl.get("Add foreign subject");
    }
}

package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.ico.ui.component.IcoDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

public class SubjectPickTypeDialog extends AppDialog {

    private final AppInfiniteGrid<SubjectDto> grid;
    private final SubjectComponentOperation subjectComponentOperation;
    private final AppEnv appEnv;
    private final ListService listService;

    public SubjectPickTypeDialog(AppInfiniteGrid<SubjectDto> grid, SubjectComponentOperation subjectComponentOperation,
                                 AppEnv appEnv, ListService listService) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.subjectComponentOperation = subjectComponentOperation;
        this.listService = listService;
        init();
    }

    private void init() {
        setTitle(Transl.get("Local or international subject"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Button local = VaadinComponents.getButton(Transl.get("Find in register"));
        Button international = VaadinComponents.getButton(Transl.get("Foreign subject"));
        local.setWidth("10em");
        local.setHeight("5em");
        international.setWidth("10em");
        international.setHeight("5em");
        horizontalLayout.add(local, international);
        setContent(horizontalLayout);
        addCloseButton();

        local.setDisableOnClick(true);
        local.addClickListener(e -> {
            new IcoDialog(grid, subjectComponentOperation, true, appEnv, listService).open();
            close();
        });

        international.setDisableOnClick(true);
        international.addClickListener(e -> {
            SubjectDto subjectDto = new SubjectDto();
            subjectDto.setLocalSubject(false);
            new SubjectNewDialog(grid, subjectComponentOperation, subjectDto, appEnv, listService).open();
            close();
        });
    }
}

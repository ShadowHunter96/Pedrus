package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.UI;
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
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;

public class SubjectAlreadyExistsDialog extends AppDialog {

    private final AppInfiniteGrid<SubjectDto> grid;
    private final SubjectComponentOperation subjectComponentOperation;
    private final ListService listService;
    private final AppEnv appEnv;
    private final String subjectId;

    public SubjectAlreadyExistsDialog(AppInfiniteGrid<SubjectDto> grid,
                                      SubjectComponentOperation subjectComponentOperation,
                                      AppEnv appEnv, ListService listService, String subjectId) {
        this.grid = grid;
        this.appEnv = appEnv;
        this.subjectComponentOperation = subjectComponentOperation;
        this.listService = listService;
        this.subjectId = subjectId;
        init();
    }

    private void init() {
        setTitle(Transl.get("Subject already exists"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        Button findAgain = VaadinComponents.getButton(Transl.get("Find different subject"));
        Button goToSubject = VaadinComponents.getButton(Transl.get("Go to subject"));
        findAgain.setWidth("10em");
        findAgain.setHeight("5em");
        goToSubject.setWidth("10em");
        goToSubject.setHeight("5em");
        horizontalLayout.add(findAgain, goToSubject);
        setContent(horizontalLayout);
        addCloseButton();

        findAgain.addClickListener(e -> {
            new IcoDialog(grid, subjectComponentOperation, true, appEnv, listService).open();
            close();
        });

        goToSubject.addClickListener(e -> {
            UI.getCurrent().navigate(SubjectDetailView.ROUTE + "/" + subjectId);
            close();
        });
    }
}

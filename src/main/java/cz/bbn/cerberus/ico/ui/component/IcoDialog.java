package cz.bbn.cerberus.ico.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

public class IcoDialog extends AppDialog {

    private final AppInfiniteGrid<SubjectDto> grid;
    private final SubjectComponentOperation subjectComponentOperation;
    private final boolean local;
    private final AppEnv appEnv;
    private final ListService listService;

    public IcoDialog(AppInfiniteGrid<SubjectDto> grid, SubjectComponentOperation subjectComponentOperation,
                     boolean local, AppEnv appEnv, ListService listService) {
        this.grid = grid;
        this.subjectComponentOperation = subjectComponentOperation;
        this.local = local;
        this.appEnv = appEnv;
        this.listService = listService;
        init();
    }

    private void init() {
        FormLayout formLayout = new FormLayout();

        setTitle(Transl.get("Find subject by name or ICO"));

        TextField nameField = new TextField(Transl.get("Name"));
        nameField.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        TextField icoField = new TextField(Transl.get("ICO"));
        icoField.setWidth(CssVariables.DEFAULT_FIELD_WIDTH.getValue());

        IcoGridComponent icoGridComponent = new IcoGridComponent(
                null, this, grid, subjectComponentOperation, local, appEnv, listService);
        icoGridComponent.setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        search.setDisableOnClick(true);
        search.addClickListener(e -> {
            icoGridComponent.loadData(subjectComponentOperation.getIcoDtoList(
                    icoField.getValue(), nameField.getValue()));
            search.setEnabled(true);
        });

        formLayout.add(nameField, icoField, search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        setContent(formLayout, icoGridComponent);

        Button cancel = VaadinComponents.getCloseButton();
        cancel.addClickListener(e -> this.close());

        addButtons(cancel);
    }
}

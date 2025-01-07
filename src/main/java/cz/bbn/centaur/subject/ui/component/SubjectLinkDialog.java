package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectType;

public class SubjectLinkDialog extends AppDialog {

    private final String title;
    private final SubjectComponentOperation subjectComponentOperation;
    private final AppEnv appEnv;
    private final String objectId;
    private final ObjectType objectType;
    private final SubjectGridComponent tabSubjectGridComponent;

    public SubjectLinkDialog(String title, SubjectComponentOperation subjectComponentOperation, AppEnv appEnv,
                             String objectId, ObjectType objectType, SubjectGridComponent tabSubjectGridComponent) {
        this.title = title;
        this.subjectComponentOperation = subjectComponentOperation;
        this.appEnv = appEnv;
        this.objectId = objectId;
        this.objectType = objectType;
        this.tabSubjectGridComponent = tabSubjectGridComponent;
        initComponent();
    }

    private void initComponent() {
        removeAll();
        setTitle(title);
        Button searchButton = VaadinComponents.getSearchButton();
        SubjectFilterComponent filterComponent = new SubjectFilterComponent(searchButton, subjectComponentOperation.getUserList(), SubjectType.SUPPLIER);
        SubjectGridComponent grid = new SubjectGridComponent(
                subjectComponentOperation.getItemsAction(filterComponent), appEnv, null);
        grid.loadData();
        searchButton.addClickListener(e -> grid.loadData());
        grid.allowMultiselect();

        setContent(filterComponent, grid);

        Button submit = VaadinComponents.getSubmitButton();
        submit.setDisableOnClick(true);
        submit.addClickListener(buttonClickEvent -> {
            subjectComponentOperation.linkSubject(grid.getSelectedItems(), objectId, objectType);
            this.close();
            tabSubjectGridComponent.loadData();
            submit.setEnabled(true);
        });

        addCloseButton();
        addSubmitButton(submit);
    }
}

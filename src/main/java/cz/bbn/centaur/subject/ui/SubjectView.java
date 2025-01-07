package cz.bbn.cerberus.subject.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.ui.component.SubjectFilterComponent;
import cz.bbn.cerberus.subject.ui.component.SubjectGridComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = SubjectView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.SUBJECT_VIEW)
@Slf4j
public class SubjectView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "subject-list";

    private final SubjectService subjectService;
    private final SubjectComponentOperation subjectComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public SubjectView(SubjectService subjectService, SubjectComponentOperation subjectComponentOperation,
                       AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.subjectService = subjectService;
        this.subjectComponentOperation = subjectComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        setSizeFull();
    }

    private void initView(String params) {
        removeAll();
        Button searchButton = VaadinComponents.getSearchButton();
        SubjectFilterComponent filterComponent = new SubjectFilterComponent(
                searchButton, subjectComponentOperation.getUserList(), params, getHistoryBreadcrumbs());
        SubjectGridComponent grid = new SubjectGridComponent(getDeleteAction(),
                subjectComponentOperation.getItemsAction(filterComponent), appEnv, null);
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Subject list"),
                Permission.SUBJECT_EDIT, Transl.get("Add subject"),
                entityNewComponentOperation.getNewSubjectEvent(grid),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        card.setId(RobotFrameworkVariables.SUBJECT_VIEW_CARD_ID.getValue());
        card.add(filterComponent);
        grid.loadData();
        searchButton.addClickListener(e -> {
            grid.loadData();
            filterComponent.fillUrl();
        });
        card.add(grid);

        add(card);
    }


    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                subjectService.deleteSubject(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}

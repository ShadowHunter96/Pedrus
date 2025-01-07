package cz.bbn.cerberus.email.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Route(value = EmailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.EMAIL_VIEW)
@Slf4j
public class EmailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "email-list";

    private final EmailComponentOperations emailComponentOperations;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public EmailView(EmailComponentOperations emailComponentOperations,
                     EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.emailComponentOperations = emailComponentOperations;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(String params) {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        EmailFilterComponent filterComponent =
                new EmailFilterComponent(search, emailComponentOperations, params, getHistoryBreadcrumbs());

        EmailGridComponent grid = new EmailGridComponent(emailComponentOperations.getDeleteAction(), appEnv,
                emailComponentOperations.getItemsAction(filterComponent), emailComponentOperations);

        AppCardGridComponent card = new AppCardGridComponent(
                Transl.get("Email list"), Permission.EMAIL_EDIT, Transl.get("Import email"),
                emailComponentOperations.getOpenImportClickEvent(grid), entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.SALES));
        search.addClickListener(e -> {
            grid.loadData();
            filterComponent.fillUrl();
        });

        card.add(filterComponent, grid);
        add(card);

    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String params) {
        initView(params);
    }
}

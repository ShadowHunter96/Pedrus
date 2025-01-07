package cz.bbn.cerberus.subject.ui.component.tab;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H4;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.ui.component.EmailFilterComponent;
import cz.bbn.cerberus.email.ui.component.EmailGridComponent;
import cz.bbn.cerberus.subject.dto.SubjectDto;

public class SubjectEmailTab extends TabSimpleComponent {

    private final SubjectDto subjectDto;
    private final EmailComponentOperations emailComponentOperations;
    private final AppEnv appEnv;

    private EmailGridComponent grid;

    public SubjectEmailTab(SubjectDto subjectDto, EmailComponentOperations emailComponentOperations, AppEnv appEnv) {
        this.subjectDto = subjectDto;
        this.emailComponentOperations = emailComponentOperations;
        this.appEnv = appEnv;
        initTab();
    }

    private void initTab() {
        Button search = VaadinComponents.getSearchButton();
        EmailFilterComponent filterComponent = new EmailFilterComponent(search, emailComponentOperations,
                DomainEnum.SUBJECT_DOMAIN_NAME, subjectDto.getId());

        grid = new EmailGridComponent(emailComponentOperations.getDeleteAction(), appEnv,
                emailComponentOperations.getItemsAction(filterComponent), emailComponentOperations);

        search.addClickListener(e -> grid.loadData());
        this.add(filterComponent, grid);
        this.setSizeFull();
    }

    @Override
    public void loadTab() {
        grid.loadData();
    }
}

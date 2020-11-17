import UIKit

class TopBarController: UIViewController {

    let StackView = TopBar(frame: CGRect.zero)
    let viewControllers: [UIViewController]
    var prevIndex = -1
    var curIndex = 0

    convenience init(_ viewControllers: UIViewController...) {
        self.init(viewControllers: viewControllers)
        commonInit()
    }

    init(viewControllers: [UIViewController]) {
        self.viewControllers = viewControllers
        super.init(nibName: nil, bundle: nil)
        commonInit()
    }
    
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    private func commonInit() {

    }

    /** Загружаем views, которыми будет управлять контроллер. Вызывается только один раз - при создании контроллера */
    override func loadView() {
        super.loadView()
        
        view = UIView()
        view.insertSubview(StackView, at: viewControllers.count)
        for i in 0..<viewControllers.count {
            addChild(viewControllers[i])
            view.insertSubview(viewControllers[i].view!, at: i)
            viewControllers[i].didMove(toParent: self)
            
            let element = TopBarButton()
            element.updateLabel(text: viewControllers[i].topBarItem!.title)
            element.updateIcon(image: viewControllers[i].topBarItem!.icon)
            element.updateIndex(index: i)
            StackView.addArrangedSubview(element)
        }
    }
    
    /** Вызывается сразу после loadView(). После того как примерная иерархия была выстроена в loadview() мы можем задать параметры и границы применимости наших views*/
    override func viewDidLoad() {
        super.viewDidLoad()
        
        for i in 0..<StackView.subviews.count {
            let element = StackView.subviews[i] as! TopBarButton
            element.button.tag = element.index
            element.button.addTarget(self, action: #selector(buttonDidTap), for:
                                        .touchUpInside)

            view.subviews[i].isHidden = true
        }
    }
    
    @objc func buttonDidTap(button: UIButton!) {
        print(button.tag)
        prevIndex = curIndex
        curIndex = button.tag
        view.setNeedsLayout()
    }
    
    /**Более точная настройка наших views. Так же именно здесь мы будем вносить изменения в наши views */
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()
        
        for i in 0..<viewControllers.count {
            view.subviews[i].frame = view.bounds
        }
        if (prevIndex != -1) {
            view.subviews[prevIndex].isHidden = true
        }
        view.subviews[curIndex].isHidden = false
        print("cur = \(curIndex)")

        let offsetUp = 40
        let offsetRight = 10
        let offsetLeft = offsetRight * 2
        StackView.frame = CGRect(x: offsetRight, y: offsetUp, width: Int(view.bounds.width) - offsetLeft, height: 65)
        StackView.spacing = (StackView.bounds.size.width - 50 * 5) / 4
    }
}

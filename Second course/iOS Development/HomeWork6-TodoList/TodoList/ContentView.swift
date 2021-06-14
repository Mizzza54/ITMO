import SwiftUI

struct TodoListView: View {
    @StateObject var viewModel = TodoListViewModel()
    
    var body: some View {
        
        VStack {
            VStack(alignment: .center) {
                TextField("Напиши в меня", text: $viewModel.text)
                Button("Добавить", action: {viewModel.addTask(text: viewModel.text)})
            }
            List {
                ForEach(0..<viewModel.sizeArray, id: \.self) { i in
                    ZStack {
                        if (viewModel.isStrikethrough[i]) {
                            Text("\(viewModel.arrayOfTasks[i])").strikethrough().foregroundColor(.gray)
                        } else {
                            Text("\(viewModel.arrayOfTasks[i])")
                        }
                        Button("", action: {viewModel.clickOnTask(index: i)})
                    }
                 }
            }.padding(.top, 50)
        }

    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            TodoListView()
        }
    }
}

//Identifiable.self
